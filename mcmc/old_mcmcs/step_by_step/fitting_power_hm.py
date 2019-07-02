import corner
import emcee
import scipy.optimize as op
import numpy as np
import matplotlib.pyplot as plt
import realMf
p_st = 0.3
q_st = 0.707
a = 1.0
N = 1000
kmin = 1e-2
kmax = 1e1
x = np.exp(np.linspace(np.log(kmin), np.log(kmax), N))

# writting mass axis to a file for fortran code

ks = open("data/k_range.dat", "w")
for k in x:
    ks.write(str(k)+"\n")
ks.close()

y = realMf.power_spectrumv(N, q_st, p_st, 1.0)
yerr = y*np.random.randn(N)
y += 0.1*yerr

# Making sure that y is always greater than 0 (density function)
# y = np.abs(y)
# print(nu)
# print(y)
# print(yerr)

# TO CHANGE !!
# Considering error as gaussian error

print(y)


def lnlike(theta, k, y, yerr, a):
    q, p = theta
    model = realMf.power_spectrumv(k, q, p, a)
    inv_sigma2 = 1.0/((0.1)*yerr)**2
    return -0.5*(np.sum((y-model)**2*inv_sigma2 - np.log(inv_sigma2)))


# Find the maximum of the likehood to get the best parameters thanks to the data

nll = lambda *args: -lnlike(*args)
result = op.minimize(nll, [q_st, p_st], args=(N, y, yerr, a))
q_ml, p_ml = result["x"]

print(q_ml, p_ml)

# MCMC, to get uncertainties
# uniform distribution of theta in a 2-D box


def lnprior(theta):
    q, p = theta
    if 0.0 < q < 1.0 and 0.0 < p < 0.45:
        return 0.0
    return -np.inf


def lnprob(theta, nu, y, yerr, a):
    lp = lnprior(theta)
    if not np.isfinite(lp):
        return -np.inf
    return lp + lnlike(theta, nu, y, yerr, a)

# Sampling distribution


ndim, nwalkers = 2, 10

# initial position of walkers (near real solution)
pos = [result["x"] + 1e-4*np.random.randn(ndim) for i in range(nwalkers)]
# pos = [[q_st, p_st] + 1e-4*np.random.randn(ndim) for i in range(nwalkers)]


sampler = emcee.EnsembleSampler(
    nwalkers, ndim, lnprob, args=(N, y, yerr, a), threads=1)
# direct computing, near maximums of the distribution
sampler.run_mcmc(pos, 100)

# removing first steps
samples = sampler.chain[:, :, :].reshape((-1, ndim))
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
