import emcee
import scipy.optimize as op
import numpy as np
import sys
import correlation
import CFHTLenS.get as data
import os


# Call this script with three arguments : icosmo, ihm and number of threads to use in parallelisation for MCMC


# Running code parameters
optimize = True
MCMC = False
verb = False

if len(sys.argv) != 4:
    print("Expecting 3 arguments : icosmo, ihm and the number of threads you want to use")
    quit()


threads = int(sys.argv[3])

# HM code parameters

icosmo = int(sys.argv[1])
ihm = int(sys.argv[2])

# log space
mmin_st = 14.0

# MCMC parameters
ndim, nwalkers, steps = 1, 200, 1000

# Importing Data from CFHT
print("Loading data")

# Length of CFHT thetas data
N = 21
x = data.thetas()

# Data from CFHTLenS survey
xip = data.xip()
xim = data.xim()
y = xip.copy()
y = np.append(y, xim)

# Considering the real covariance matrix and all kind of errors
yerr = data.cov_mat()
yerrinv = np.linalg.inv(yerr)
det = np.linalg.det(yerr)
errp = data.sigp()
errm = data.sigm()


def lnlike(param, y, invcov, verbose=False):
    mmin = param[0]
    if not (7. <= mmin <= 15.):
        return -np.inf
    model = correlation.xi_mminCFHT(mmin, icosmo, ihm, verbose=verbose)
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
    print("Starting fitting mmin")
    # We want to maximise the likehood
    nll = lambda *args: -lnlike(*args)
    # Best to use log-parameters I think, thanks to the living spaces of p and q
    result = op.minimize(nll, [mmin_st],
                         args=(y, yerrinv, True), method='Nelder-Mead', tol=1e-6)
    mmin_ml = result["x"][0]
    print("Best fit is mmin={0}" .format(mmin_ml))
    data = open("mcmc/results/CFHT/ihm={1}/mmin{0}.txt" .format(*[icosmo, ihm]), "w")
    data.write("mmin={0}" .format(mmin_ml))
    data.close()

else:
    if os.path.isfile("mcmc/results/CFHT/ihm={1}/mmin{0}.txt" .format(*[icosmo, ihm])):
        data = open("mcmc/results/CFHT/ihm={1}/mmin{0}.txt" .format(*[icosmo, ihm]), "r")
        line = data.readlines()
        data.close()
        mmin_ml = float(line.split('=')[1])
    else:
        mmin_ml = mmin_st

# MCMC, to get uncertainties
# uniform distribution of theta in a 2-D box

if not MCMC:
    quit()


def lnprior(param):
    mmin = param[0]
    if 7. <= mmin <= 15.:
        return 0.0
    return -np.inf


def lnprob(param, y, invcov, verbose=False):
    lp = lnprior(param)
    if not np.isfinite(lp):
        return -np.inf
    return lp + lnlike(param, y, invcov, verbose)


print("Starting MCMC on {0} threads" .format(threads))


# initial position of walkers (near real solution)
pos = [[mmin_st] + 1e-2*np.random.randn(ndim) for i in range(nwalkers)]

if optimize:
    pos = [[mmin_ml] + 1e-2*np.random.randn(ndim) for i in range(nwalkers)]

sampler = emcee.EnsembleSampler(
    nwalkers, ndim, lnprob, args=(y, yerrinv, verb), threads=threads)
# direct computing, near maximums of the distribution
sampler.run_mcmc(pos, steps)

# save data
np.save('mcmc/results/CFHT/ihm={1}/mmin_chain{0}.npy' .format(*[icosmo, ihm]), sampler.chain)

data = open("mcmc/results/CFHT/ihm={1}/mmin_acceptanceFraction{0}.txt" .format(*[icosmo, ihm]), "w")
data.write("\n\nMean acceptance fraction: {0:.3f}" .format(np.mean(sampler.acceptance_fraction)))
data.close()
