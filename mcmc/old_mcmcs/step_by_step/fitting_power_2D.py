import corner
import emcee
import scipy.optimize as op
import numpy as np
import matplotlib.pyplot as plt
import realMf
import os

# Running code parameters
parallel = True
threads = 8
optimize = False

# model parameters
p_st = 0.3
q_st = 0.707

# MCMC parameters
ndim, nwalkers = 2, 30

# Fake data
N = 40
lmin = 100
lmax = 5000
x = np.exp(np.linspace(np.log(lmin), np.log(lmax), N))

# writting mass axis to a file for fortran code

ls = open("data/l_range.dat", "w")
for l in x:
    ls.write(str(l)+"\n")
ls.close()

y = realMf.power_spectrum2Dv(N, q_st, p_st, parallel=False)
yerr = y*np.random.randn(N)
y += 0.1*yerr

# Making sure that y is always greater than 0 (density function)
# y = np.abs(y)
# print(nu)
# print(y)
# print(yerr)

# TO CHANGE !!
# Considering error as gaussian error

# print(y)


def lnlike(theta, l, y, yerr, parallel):
    q, p = theta
    model = realMf.power_spectrum2Dv(l, q, p, parallel=parallel)
    inv_sigma2 = 1.0/((0.1)*yerr)**2
    return -0.5*(np.sum((y-model)**2*inv_sigma2 - np.log(inv_sigma2)))


# Find the maximum of the likehood to get the best parameters thanks to the data

if optimize:
    nll = lambda *args: -lnlike(*args)
    result = op.minimize(nll, [q_st, p_st], args=(N, y, yerr, False))
    q_ml, p_ml = result["x"]

    print(q_ml, p_ml)
else:
    q_ml, p_ml = q_st, p_st
    print("No optimisation, only MCMC")
    print(q_ml, p_ml)
# MCMC, to get uncertainties
# uniform distribution of theta in a 2-D box


def lnprior(theta):
    q, p = theta
    if 0.0 < q < 1.0 and 0.0 < p < 0.45:
        return 0.0
    return -np.inf


def lnprob(theta, l, y, yerr, parallel):
    lp = lnprior(theta)
    if not np.isfinite(lp):
        return -np.inf
    return lp + lnlike(theta, l, y, yerr, parallel)

# Sampling distribution


# setting up for parallel
if parallel:
    # cleaning old data files
    os.system("rm -rf data/data*")

    # generating copies of the binary file
    for i in range(50):
        os.system("cp ./bin/halo_model.2D ./bin/halo_model.2D.{0}" .format(i))


# initial position of walkers (near real solution)
pos = [[q_ml, p_ml] + 1e-4*np.random.randn(ndim) for i in range(nwalkers)]
# pos = [[q_st, p_st] + 1e-4*np.random.randn(ndim) for i in range(nwalkers)]


sampler = emcee.EnsembleSampler(
    nwalkers, ndim, lnprob, args=(N, y, yerr, parallel), threads=threads)
# direct computing, near maximums of the distribution
sampler.run_mcmc(pos, 250)

# removing first steps
samples = sampler.chain[:, 100:, :].reshape((-1, ndim))
# plots of the sampler
plt.figure(1)
for i in range(nwalkers):
    ax1 = plt.subplot(311)
    ax1.plot(sampler.chain[i, :, 0], color='black')
    ax2 = plt.subplot(312, sharex=ax1)
    ax2.plot(sampler.chain[i, :, 1], color='black')
    # ax3=plt.subplot(313, sharex=ax1)
    # ax3.plot(sampler.chain[i,:,2], color='black')

plt.figure(1)
plt.show()

fig = corner.corner(samples, labels=["$q$", "$p$"], truths=[q_st, p_st])
plt.show(fig)
fig.savefig("triangle.png")
