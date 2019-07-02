import numpy as np
import matplotlib.pyplot as plt
import mf

p_st=0.3
q_st=0.707
A_st=mf.normalisation(q_st, p_st)
N=100
yerr=0.01*np.random.randn(N)
nu=0.1+np.abs(5*np.random.rand(N))
y=mf.unprobv(nu, q_st, p_st)*A_st
y+=yerr

#Making sure that y is always greater than 0 (density function)
y=np.abs(y)
# print(nu)
# print(y)
# print(yerr)

## TO CHANGE !!
# Considering error as gaussian error
def lnlike(theta, nu, y, yerr):
	q, p = theta
	model = mf.unprobv(nu,q,p)*mf.normalisation(q,p)
	inv_sigma2 = 1.0/yerr**2
	return -0.5*(np.sum((y-model)**2*inv_sigma2 - np.log(inv_sigma2)))


#Find the maximum of the likehood to get the best parameters thanks to the data

import scipy.optimize as op
nll = lambda *args: -lnlike(*args)
result = op.minimize(nll, [q_st, p_st], args=(nu, y, yerr))
q_ml, p_ml = result["x"]

print(q_ml, p_ml)

## MCMC, to get uncertainties
# uniform distribution of theta in a 2-D box
def lnprior(theta):
	q, p = theta
	if 0.0 < q < 1.0 and 0.0 < p < 0.45:
		return 0.0
	return -np.inf

def lnprob(theta, nu, y, yerr):
	lp = lnprior(theta)
	if not np.isfinite(lp):
		return -np.inf
	return lp + lnlike(theta, nu, y, yerr)

## Sampling distribution

ndim, nwalkers=2, 100

#initial position of walkers (near real solution)
pos=[result["x"] + 1e-4*np.random.randn(ndim) for i in range(nwalkers)]

import emcee
sampler = emcee.EnsembleSampler(nwalkers, ndim, lnprob, args=(nu, y, yerr), threads=8)
# direct computing, near maximums of the distribution
sampler.run_mcmc(pos, 500)

#removing first steps
samples = sampler.chain[:, 50:, :].reshape((-1, ndim))
# plots of the sampler
plt.figure(1)
for i in range(nwalkers):
	ax1=plt.subplot(311)
	ax1.plot(sampler.chain[i,:,0], color='black')
	ax2=plt.subplot(312, sharex=ax1)
	ax2.plot(sampler.chain[i,:,1], color='black')
	# ax3=plt.subplot(313, sharex=ax1)
	# ax3.plot(sampler.chain[i,:,2], color='black')

plt.figure(1)
plt.show()

import corner
fig = corner.corner(samples, labels=["$q$", "$p$"], truths=[q_st, p_st])
plt.show(fig)
fig.savefig("triangle.png")