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
parallel = True
optimize = True
MCMC = False
threads = 1
if parallel:
    threads = 8

# model parameters
p_st = 0.3
q_st = 0.707

p_o = 0.2
q_o = 0.6

p_st_log = - np.log(p_st)
q_st_log = - np.log(q_st)


p_o_log = - np.log(p_o)
q_o_log = - np.log(q_o)

# MCMC parameters
ndim, nwalkers, steps, firsts = 2, 100, 250, 50

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
    q_log, p_log = theta_log
    # q = np.exp(-q_log)
    # p = np.exp(-p_log)
    q = q_log
    p = p_log
    if not (0.5 < q < 1.0 and 0.1 < p < 0.45):
        return -np.inf
    model = realMf.CFHTv(nth, q, p, parallel=par)
    disk = 1
    while model[0] == np.pi:
        model = realMf.CFHTv(nth, q, p, parallel=par)
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
    print("Starting fitting p and q")
    # We want to maximise the likehood
    nll = lambda *args: -lnlike(*args)
    # Best to use log-parameters I think, thanks to the living spaces of p and q
    result = op.minimize(nll, [q_st, p_st],
                         args=(N, y, yerrinv, False))
    q_ml_log, p_ml_log = result["x"]
    # q_ml = np.exp(-q_ml_log)
    # p_ml = np.exp(-p_ml_log)
    q_ml = q_ml_log
    p_ml = p_ml_log
    print(q_ml, p_ml)
    data = open("results.txt", "w")
    data.write("q={0}, p={1}" .format(*[q_ml, p_ml]))
    data.close()

    # plot model with results

    os.system("mkdir -p figures/CFHT")
    model = realMf.CFHTv(N, q_ml, p_ml)
    theory = realMf.CFHTv(N, q_st, p_st)
    plt.figure(1).set_size_inches((8, 8), forward=False)
    plt.title(
        "Correlation function $\\xi_+$")
    plt.plot(x, model[:N], label="CFHTLenS")
    plt.plot(x, theory[:N], '--', label="Sheth and Tormen")
    plt.legend()

    plt.errorbar(x, xip, np.sqrt(errp), fmt='o')
    plt.xscale('log')
    plt.yscale('log')
    plt.xlabel('$\\theta (arcmin)$')
    plt.ylabel('$\\xi_+$')
    plt.savefig('figures/CFHT/xip.png', bbox_inches='tight')

    plt.figure(2).set_size_inches((8, 8), forward=False)
    plt.title(
        "Correlation function $\\xi_-$")
    plt.plot(x, model[N:], label="CFHTLenS")
    plt.plot(x, theory[N:], '--', label="Sheth and Tormen")
    plt.legend()

    plt.errorbar(x, xim, np.sqrt(errm), fmt='o')
    plt.xscale('log')
    plt.yscale('log')
    plt.xlabel('$\\theta (arcmin)$')
    plt.ylabel('$\\xi_-$')
    plt.savefig('figures/CFHT/xim.png', bbox_inches='tight')
    # plt.show()
else:
    q_ml, p_ml = q_st, p_st


# MCMC, to get uncertainties
# uniform distribution of theta in a 2-D box

if not MCMC:
    quit()


def lnprior(theta):
    q, p = theta
    if 0.5 < q < 1.0 and 0.0 < p < 0.49:
        return 0.0
    return -np.inf


def lnprob(theta, nth, y, invcov, par):
    lp = lnprior(theta)
    if not np.isfinite(lp):
        return -np.inf
    # theta_log = -np.log(theta)
    return lp + lnlike(theta_log, nth, y, invcov, par)


print("Starting MCMC")

# Sampling distribution

# setting up for parallel
if parallel:
    # cleaning old data files
    os.system("rm -rf data/data*")

    # generating copies of the binary file
    if not os.path.isfile("./bin/halo_model.CFHT1.4999"):
        for i in range(5000):
            os.system(
                "cp bin/halo_model.CFHT1 bin/halo_model.CFHT1.{0}" .format(i))


# initial position of walkers (near real solution)
pos = [[q_st, p_st] + 1e-4*np.random.randn(ndim) for i in range(nwalkers)]
if optimize:
    pos = [[q_ml, p_ml] + 1e-4*np.random.randn(ndim) for i in range(nwalkers)]


sampler = emcee.EnsembleSampler(
    nwalkers, ndim, lnprob, args=(N, y, yerrinv, parallel), threads=threads)
# direct computing, near maximums of the distribution
sampler.run_mcmc(pos, steps)


# save data
np.save('data/mcmc.npy', sampler.chain)

# removing first steps
samples = sampler.chain[:, firsts:, :].reshape((-1, ndim))
# plots of the sampler
plt.figure(3)
for i in range(nwalkers):
    ax1 = plt.subplot(311)
    ax1.plot(sampler.chain[i, :, 0], color='black')
    ax2 = plt.subplot(312, sharex=ax1)
    ax2.plot(sampler.chain[i, :, 1], color='black')
    # ax3=plt.subplot(313, sharex=ax1)
    # ax3.plot(sampler.chain[i,:,2], color='black')

plt.figure(4)
plt.savefig("mcmc_walkers_CFHT.png")
# plt.show()

fig = corner.corner(samples, labels=["$q$", "$p$"], truths=[q_st, p_st])
fig.savefig("mcmc_CFHT.png")
plt.show(fig)
