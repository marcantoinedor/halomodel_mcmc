import corner
import emcee
import scipy.optimize as op
import numpy as np
import sys
import correlation
import CFHTLenS.get as data
import os


# Call this script with one argument : number of threads to use in parallelisation for MCMC


# Running code parameters
optimize = False
MCMC = True
verb = False

if len(sys.argv) != 3:
    print("Expecting 2 arguments : icosmo and the number of threads you want to use")
    quit()


threads = int(sys.argv[2])

# HM code parameters

icosmo = int(sys.argv[1])
ihm = 3

# model parameters
p_st = 0.3
q_st = 0.707

# others beginning_points
p_o = 0.2
q_o = 0.6

# MCMC parameters
ndim, nwalkers, steps = 2, 200, 1000

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


def lnlike(params, y, invcov, verbose=False):
    q, p = params
    if not (0.2 < q < 1.5 and 0.0 <= p < 0.5):
        return -np.inf
    model = correlation.xiCFHT(q, p, icosmo, ihm, verbose=verbose)
    if model[0] == -np.pi:
        return -np.inf
    return -0.5*np.matmul(np.transpose(y-model), np.matmul(invcov, (y-model)))


if not MCMC:
    print("Fitting model only mode")
if not optimize:
    print("MCMC only mode, using already computed parameters")

# Find the maximum of the likehood to get the best parameters thanks to the data
os.system("mkdir -p mcmc/results/CFHT")

if optimize:
    print("Starting fitting p and q")
    # We want to maximise the likehood
    nll = lambda *args: -lnlike(*args)
    # Best to use log-parameters I think, thanks to the living spaces of p and q
    result = op.minimize(nll, [q_st, p_st], args=(y, yerrinv, True))
    q_ml, p_ml = result["x"]
    print("Best fit is q={0}, p={1}" .format(*[q_ml, p_ml]))
    data = open("mcmc/results/CFHT/fit{0}.txt" .format(icosmo), "w")
    data.write("q={0}, p={1}" .format(*[q_ml, p_ml]))
    data.close()

else:
    if os.path.isfile("mcmc/results/CFHT/fit{0}.txt" .format(icosmo)):
        data = open("mcmc/results/CFHT/fit{0}.txt" .format(icosmo), "r")
        line = data.readlines()
        data.close()

        values = line[0].split(',')

        q_ml = float(values[0].split('=')[1])
        p_ml = float(values[1].split('=')[1])
    else:
        q_ml, p_ml = q_st, p_st

# MCMC, to get uncertainties
# uniform distribution of theta in a 2-D box

if not MCMC:
    quit()


def lnprior(params):
    q, p = params
    if 0.2 < q < 1.5 and 0.0 <= p < 0.5:
        return 0.0
    return -np.inf


def lnprob(params, y, invcov, verbose=False):
    lp = lnprior(params)
    if not np.isfinite(lp):
        return -np.inf
    return lp + lnlike(params, y, invcov, verbose)


print("Starting MCMC on {0} threads" .format(threads))


# initial position of walkers (near real solution)
pos = [[q_st, p_st] + 1e-2*np.random.randn(ndim) for i in range(nwalkers)]

if optimize:
    pos = [[q_ml, p_ml] + 1e-2*np.random.randn(ndim) for i in range(nwalkers)]

sampler = emcee.EnsembleSampler(
    nwalkers, ndim, lnprob, args=(y, yerrinv, verb), threads=threads)
# direct computing, near maximums of the distribution
sampler.run_mcmc(pos, steps)

# save data
np.save('mcmc/results/CFHT/chain{0}.npy' .format(icosmo), sampler.chain)

data = open("mcmc/results/CFHT/acceptanceFraction{0}.txt" .format(icosmo), "w")
data.write("Mean acceptance fraction: {0:.3f}" .format(np.mean(sampler.acceptance_fraction)))
data.close()
