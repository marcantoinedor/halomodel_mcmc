import emcee
import scipy.optimize as op
import numpy as np
import sys
import correlation
import CFHTLenS.get as dat
import os


# Call this script with three arguments : icosmo, ihm and number of threads to use in parallelisation for MCMC


# Running code parameters
optimize = True
MCMC = True
verb = False

if len(sys.argv) != 4:
    print("Expecting 3 arguments : icosmo, ihm and the number of threads you want to use")
    quit()


threads = int(sys.argv[3])

# HM code parameters

icosmo = int(sys.argv[1])
ihm = int(sys.argv[2])

alpha_st = 1.0

# MCMC parameters
ndim, nwalkers, steps = 1, 200, 1000

# Importing Data from CFHT
print("Loading data")

# Length of CFHT thetas data
N = 21
x = dat.thetas()

# Data from CFHTLenS survey
xip = dat.xip()
xim = dat.xim()
y = xip.copy()
y = np.append(y, xim)

# Considering the real covariance matrix and all kind of errors
yerr = dat.cov_mat()
yerrinv = np.linalg.inv(yerr)
det = np.linalg.det(yerr)
errp = dat.sigp()
errm = dat.sigm()


def lnlike(param, y, invcov, verbose=False):
    alpha = param[0]
    if not (0.5 < alpha < 1.5):
        return -np.inf
    model = correlation.xi_ampCFHT(alpha, icosmo, ihm, verbose=verbose)
    if model[0] == -np.pi:
        return -np.inf
    return -0.5*np.matmul(np.transpose(y-model), np.matmul(invcov, (y-model)))


if not MCMC:
    print("Fitting model only mode")
if not optimize:
    print("MCMC only mode, using Sheth and Tormen parameters")

# Find the maximum of the likehood to get the best parameters thanks to the data
os.system("mkdir -p mcmc/results/CFHT")

if optimize:
    print("Starting fitting alpha")
    # We want to maximise the likehood
    nll = lambda *args: -lnlike(*args)
    # Best to use log-parameters I think, thanks to the living spaces of p and q
    result = op.minimize(nll, [alpha_st],
                         args=(y, yerrinv, True), method='Nelder-Mead', tol=1e-6)
    alpha_ml = result["x"][0]
    print("Best fit is alpha={0}" .format(alpha_ml))
    data = open("mcmc/results/CFHT/ihm={1}/alpha{0}.txt" .format(*[icosmo, ihm]), "w")
    data.write("alpha={0}" .format(alpha_ml))
    data.close()

else:
    if os.path.isfile("mcmc/results/CFHT/ihm={1}/alpha{0}.txt" .format(*[icosmo, ihm])):
        data = open("mcmc/results/CFHT/ihm={1}/alpha{0}.txt" .format(*[icosmo, ihm]), "r")
        line = data.readlines()
        data.close()
        alpha_ml = float(line.split('=')[1])
    else:
        alpha_ml = alpha_st

# MCMC, to get uncertainties
# uniform distribution of theta in a 2-D box

if not MCMC:
    quit()


def lnprior(param):
    alpha = param[0]
    if 0.5 < alpha < 1.5:
        return 0.0
    return -np.inf


def lnprob(param, y, invcov, verbose=False):
    lp = lnprior(param)
    if not np.isfinite(lp):
        return -np.inf
    return lp + lnlike(param, y, invcov, verbose)


print("Starting MCMC on {0} threads" .format(threads))


# initial position of walkers (near real solution)
pos = [[alpha_st] + 1e-2*np.random.randn(ndim) for i in range(nwalkers)]

if optimize:
    pos = [[alpha_ml] + 1e-2*np.random.randn(ndim) for i in range(nwalkers)]

sampler = emcee.EnsembleSampler(
    nwalkers, ndim, lnprob, args=(y, yerrinv, verb), threads=threads)
# direct computing, near maximums of the distribution
sampler.run_mcmc(pos, steps)

# save data
np.save('mcmc/results/CFHT/ihm={1}/alpha_chain{0}.npy' .format(*[icosmo, ihm]), sampler.chain)

data = open("mcmc/results/CFHT/ihm={1}/alpha_acceptanceFraction{0}.txt" .format(*[icosmo, ihm]), "w")
data.write("\n\nMean acceptance fraction: {0:.3f}" .format(np.mean(sampler.acceptance_fraction)))
data.close()
