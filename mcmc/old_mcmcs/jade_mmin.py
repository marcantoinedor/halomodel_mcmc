import corner
import emcee
import scipy.optimize as op
import numpy as np
import matplotlib.pyplot as plt
import realMf
import os
import sys
from emcee.utils import MPIPool

# Running code parameters
parallel = False
optimize = True
MCMC = False
threads = 1
if parallel:
    threads = 8

# model parameters
p_st = 0.3
q_st = 0.707
mmin = 15.

p_o = 0.2
q_o = 0.6
mmin_o = 7.

p_st_log = - np.log(p_st)
q_st_log = - np.log(q_st)
mmin_log = mmin


p_o_log = - np.log(p_o)
q_o_log = - np.log(q_o)
mmin_log = mmin

# MCMC parameters
ndim, nwalkers, steps, firsts = 3, 100, 250, 50

# Importing Data from CFHT
print("Loading data")
N = 21
x = np.array(realMf.get_theta_CFHT())

# Data from CFHTLenS survey
xip = realMf.get_xip_CFHT()
xim = realMf.get_xim_CFHT()
y = xip.copy()
for value in xim:
    y.append(value)
# y = [[xip[i], xim[i]] for i in range(N)]
# # Considering uncorrelated errors
# errp = realMf.get_sigp_CFHT()
# errm = realMf.get_sigm_CFHT()
# yerr = np.array([[errp[i], errm[i]]for i in range(N)])

# Considering the real covariance matrix and all kind of errors
yerr = realMf.get_cov_mat_CFHT()
yerrinv = np.linalg.inv(yerr)
det = np.linalg.det(yerr)
errp = realMf.get_sigp_CFHT()
errm = realMf.get_sigm_CFHT()


def lnlike(theta_log, nth, y, invcov, par):
    q_log, p_log, mmin_log = theta_log
    # q = np.exp(-q_log)
    # p = np.exp(-p_log)
    q = q_log
    p = p_log
    mmin = mmin_log
    if not (0.5 < q < 1.0 and 0.1 < p < 0.45 and mmin > 7):
        return -np.inf
    model = realMf.CFHTmminv(nth, q, p, mmin, parallel=par)
    disk = 1
    while model[0] == np.pi:
        model = realMf.CFHTmminv(nth, q, p, mmin, parallel=par)
        print("probem on disk, iteration : {0}" .format(disk))
        disk += 1
    if model[0] == -np.pi:
        return -np.inf
    return -0.5*np.matmul(np.transpose(y-model), np.matmul(invcov, (y-model)))


if not MCMC:
    print("Fitting model only mode")
if not optimize:
    print("MCMC only mode, using Sheth and Tormen parameters")

# Find the maximum of the likehood to get the best parameters thanks to the data

if optimize:
    print("Starting fitting p, q, and mmin")
    # We want to maximise the likehood
    nll = lambda *args: -lnlike(*args)
    # Best to use log-parameters I think, thanks to the living spaces of p and q
    result = op.minimize(nll, [q_o, p_o, mmin], args=(N, y, yerrinv, False))
    q_ml_log, p_ml_log, mmin_log = result["x"]
    # q_ml = np.exp(-q_ml_log)
    # p_ml = np.exp(-p_ml_log)
    q_ml = q_ml_log
    p_ml = p_ml_log
    mmin_ml = mmin_log
    print(q_ml, p_ml, mmin_ml)
    data = open("results_mmin.txt", "w")
    data.write("q={0}, p={1}, log_mmin={2}" .format(*[q_ml, p_ml, mmin]))
    data.close()

else:
    q_ml, p_ml, mmin_ml = q_st, p_st, mmin

# MCMC, to get uncertainties
# uniform distribution of theta in a 2-D box

if not MCMC:
    quit()


def lnprior(theta):
    q, p = theta
    if 0.5 < q < 1.0 and 0.1 < p < 0.45 and mmin > 7:
        return 0.0
    return -np.inf


def lnprob(theta, nth, y, invcov, par):
    lp = lnprior(theta)
    if not np.isfinite(lp):
        return -np.inf
    # theta_log = -np.log(theta)
    theta_log = theta
    return lp + lnlike(theta_log, nth, y, invcov, par)


print("Starting MCMC")

# Sampling distribution

# setting up for parallel
if parallel:
    # cleaning old data files
    os.system("rm -rf data/data*")

    # generating copies of the binary file
    if not os.path.isfile("./bin/halo_model.mminCFHT.99"):
        for i in range(100):
            os.system(
                "cp bin/halo_model.mminCFHT bin/halo_model.mminCFHT.{0}" .format(i))


# initial position of walkers (near real solution)
pos = [[q_st, p_st, mmin] + 1e-4 *
       np.random.randn(ndim) for i in range(nwalkers)]
if optimize:
    pos = [[q_ml, p_ml, mmin_ml] + 1e-4 *
           np.random.randn(ndim) for i in range(nwalkers)]

sampler = emcee.EnsembleSampler(
    nwalkers, ndim, lnprob, args=(N, y, yerrinv, parallel), threads=threads)
# direct computing, near maximums of the distribution
sampler.run_mcmc(pos, steps)

# save data
np.save('data/mcmc_mmin.npy', sampler.chain)
