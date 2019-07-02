import numpy as np
import matplotlib.pyplot as plt
# Choose the "true" parameters.
m_true = -0.9594
b_true = 4.294
f_true = 0.534

def true_function(x):
	return m_true*x+b_true

truev=np.vectorize(true_function)
# Generate some synthetic data from the model.
N = 50
x = np.sort(10*np.random.rand(N))
yerr = 0.1+0.5*np.random.rand(N)
y = m_true*x+b_true
y += np.abs(f_true*y) * np.random.randn(N)
y += yerr * np.random.randn(N)

# Mean least square solution

A = np.vstack((np.ones_like(x), x)).T
C = np.diag(yerr * yerr)
cov = np.linalg.inv(np.dot(A.T, np.linalg.solve(C, A)))
b_ls, m_ls = np.dot(cov, np.dot(A.T, np.linalg.solve(C, y)))

def ls_function(x):
	return m_ls*x+b_ls

lsv=np.vectorize(ls_function)

def lnlike(theta, x, y, yerr):
	m, b, lnf = theta
	model = m * x + b
	inv_sigma2 = 1.0/(yerr**2 + model**2*np.exp(2*lnf))
	return -0.5*(np.sum((y-model)**2*inv_sigma2 - np.log(inv_sigma2)))

#Find the maximum of the likehood to get the best parameters thanks to the data

import scipy.optimize as op
nll = lambda *args: -lnlike(*args)
result = op.minimize(nll, [m_true, b_true, np.log(f_true)], args=(x, y, yerr))
m_ml, b_ml, lnf_ml = result["x"]

def ml_function(x):
	return m_ml*x+b_ml

mlv=np.vectorize(ml_function)

## Plots
# #Real model
# x_axis=np.linspace(0, 10, 1000)
# plt.plot(x_axis, truev(x_axis))
# #Infered model
# plt.plot(x_axis, lsv(x_axis), '--', label="MS")
# #Optimized model
# plt.plot(x_axis, mlv(x_axis), '--', label="ML")
# plt.legend()
# # Data
# plt.errorbar(x, y,yerr=yerr, fmt='o')

# plt.show()

def lnprior(theta):
	m, b, lnf = theta
	if -5.0 < m < 0.5 and 0.0 < b < 10.0 and -10.0 < lnf < 1.0:
		return 0.0
	return -np.inf

def lnprob(theta, x, y, yerr):
	lp = lnprior(theta)
	if not np.isfinite(lp):
		return -np.inf
	return lp + lnlike(theta, x, y, yerr)

## Sampling distribution

ndim, nwalkers=3, 100

#initial position of walkers (near real solution)
pos=[result["x"] + 1e-4*np.random.randn(ndim) for i in range(nwalkers)]

import emcee
sampler = emcee.EnsembleSampler(nwalkers, ndim, lnprob, args=(x, y, yerr))
# direct computing, near maximums of the distribution
sampler.run_mcmc(pos, 5000)

#removing first steps
samples = sampler.chain[:, 50:, :].reshape((-1, ndim))
## plots of the sampler
# plt.figure(1)
# for i in range(nwalkers):
# 	ax1=plt.subplot(311)
# 	ax1.plot(sampler.chain[i,:,0], color='black')
# 	ax2=plt.subplot(312, sharex=ax1)
# 	ax2.plot(sampler.chain[i,:,1], color='black')
# 	ax3=plt.subplot(313, sharex=ax1)
# 	ax3.plot(sampler.chain[i,:,2], color='black')

# plt.figure(1)
# plt.show()

import corner
fig = corner.corner(samples, labels=["$m$", "$b$", "$\ln\,f$"], truths=[m_true, b_true, np.log(f_true)])
plt.show(fig)
fig.savefig("triangle.png")