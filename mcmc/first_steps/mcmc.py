import numpy as np
import emcee
import matplotlib.pyplot as plt

def lnprob(x, mu, icov):
	'''
		ndarray, ndarray, ndarray
	'''
	diff = x - mu
	return -np.dot(diff, icov)/2.0

ndim = 5

means = np.random.rand(ndim)

cov = 0.5 - np.random.rand(ndim ** 2).reshape((ndim, ndim))
cov = np.triu(cov)
cov += cov.T - np.diag(cov.diagonal())
cov = np.dot(cov,cov)
icov = np.linalg.inv(cov)

nwalkers = 20
p0 = np.random.rand(ndim * nwalkers).reshape((nwalkers, ndim))

sampler = emcee.EnsembleSampler(nwalkers, ndim, lnprob, args=[means, icov])

[pos, prob, state, blobs] = sampler.run_mcmc(p0, 100)
sampler.reset()

print(pos)
print(prob)
print(state)
print(blobs)
sampler.run_mcmc(pos, 1000)

# for i in range(ndim):
# 	plt.figure()
# 	# plt.hist(sampler.flatchain[:,i], 100, color="k", histtype="step")
# 	# plt.title("Dimension {0:d}".format(i))
# 	print(min(sampler.flatchain[:,i]))
# 	for j in range(nwalkers):
# 		print(sampler.flatchain[:,i][j])
# 	plt.xscale('log')
# 	plt.yscale('log')
# 	plt.plot(sampler.flatchain[:,i],'o')
print("Mean acceptance fraction: {0:.3f}" .format(np.mean(sampler.acceptance_fraction)))

# plt.show()