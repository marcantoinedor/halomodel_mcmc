import corner
import emcee
import scipy.optimize as op
import numpy as np
import matplotlib.pyplot as plt
import realMf
import os
import sys

# MCMC parameters
ndim, nwalkers, steps, firsts = 2, 100, 250, 100
nbr = 100
others = False
lower1 = 10
lower2 = 25
print("Loading and computing data")

data = open('results.txt', "r")
line = data.readlines()
data.close()

values = line[0].split(',')

q_ml = float(values[0].split('=')[1])
p_ml = float(values[1].split('=')[1])

N = 21
q_st = 0.707
p_st = 0.3

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

chain = np.load('results/mcmc.npy')

# print(chain.size)
# print(chain)
otherchain = chain.reshape((nwalkers, steps, ndim))
# print(chain)

# removing first steps
samples = chain[:, firsts:, :].reshape((-1, ndim))

# plot model with results

os.system("mkdir -p figures/CFHT")
model = realMf.CFHTv(N, q_ml, p_ml)
theory = realMf.CFHTv(N, q_st, p_st)

print("Plotting basic results")
plt.figure(1).set_size_inches((8, 8), forward=False)
plt.title(
    "Correlation function $\\xi_+$")
plt.plot(x, model[:N], label="CFHTLenS")
plt.plot(x, theory[:N], '--', label="Sheth and Tormen")
plt.legend()
plt.errorbar(x, xip, np.sqrt(errp), fmt='.k',  elinewidth=0.5, capsize=3)
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

plt.errorbar(x, xim, np.sqrt(errm), fmt='.k',  elinewidth=0.5, capsize=3)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_-$')
plt.savefig('figures/CFHT/xim.png', bbox_inches='tight')

# plots of the sampler
plt.figure(3)
for i in range(nwalkers):
    ax1 = plt.subplot(311)
    ax1.plot(chain[i, :, 0], color='black')
    ax2 = plt.subplot(312, sharex=ax1)
    ax2.plot(chain[i, :, 1], color='black')

plt.figure(3)
plt.savefig("mcmc_walkers_CFHT.png")

fig = corner.corner(samples, labels=["$q$", "$p$"], truths=[q_ml, p_ml])
fig.savefig("CFHT/mcmc_contours.png")

print("Basic plots created")

print("Computing and ploting percentiles")
q_mcmc1, p_mcmc1 = map(lambda v: (
    v[1], v[2]-v[1], v[1]-v[0]), zip(*np.percentile(samples, [lower1, 50, 100-lower1], axis=0)))
print("MCMC found these values for {0}/100 certainties" .format(lower1))
print("q={0} + {1}, - {2}" .format(*[q_mcmc1[0], q_mcmc1[1], q_mcmc1[2]]))
print("p={0} + {1}, - {2}" .format(*[p_mcmc1[0], p_mcmc1[1], p_mcmc1[2]]))

q_mcmc2, p_mcmc2 = map(lambda v: (
    v[1], v[2]-v[1], v[1]-v[0]), zip(*np.percentile(samples, [lower2, 50, 100-lower2], axis=0)))
print("MCMC found these values for {0}/100 certainties" .format(lower2))
print("q={0} + {1}, - {2}" .format(*[q_mcmc2[0], q_mcmc2[1], q_mcmc2[2]]))
print("p={0} + {1}, - {2}" .format(*[p_mcmc2[0], p_mcmc2[1], p_mcmc2[2]]))

upper_percentile1 = realMf.CFHTv(
    N, q_mcmc1[0]+q_mcmc1[1], p_mcmc1[0]+p_mcmc1[1])
lower_percentile1 = realMf.CFHTv(
    N, q_mcmc1[0]-q_mcmc1[2], p_mcmc1[0]-p_mcmc1[2])

upper_percentile2 = realMf.CFHTv(
    N, q_mcmc2[0]+q_mcmc2[1], p_mcmc2[0]+p_mcmc2[1])
lower_percentile2 = realMf.CFHTv(
    N, q_mcmc2[0]-q_mcmc2[2], p_mcmc2[0]-p_mcmc2[2])


plt.figure(7).set_size_inches((8, 8), forward=False)
plt.title(
    "Correlation function $\\xi_+$")
plt.plot(x, model[:N], '--', color='r', label="CFHTLenS")
plt.plot(x, theory[:N], '--', color='orange', label="Sheth and Tormen")
plt.plot(x, upper_percentile1[:N], color='g', alpha=0.5,
         label="Upper value {0} %" .format(100-lower1))
plt.plot(x, lower_percentile1[:N], color='g', alpha=0.5)
plt.plot(x, upper_percentile2[:N], color='b', alpha=0.5,
         label="Border value {0} %" .format(100-lower2))
plt.plot(x, lower_percentile2[:N], color='b', alpha=0.5)
plt.legend()

plt.errorbar(x, xip, np.sqrt(errp), fmt='.k',  elinewidth=0.5, capsize=3)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_+$')


plt.savefig('figures/CFHT/xip_percentile.png', bbox_inches='tight')
plt.figure(8).set_size_inches((8, 8), forward=False)
plt.title(
    "Correlation function $\\xi_-$")
plt.plot(x, model[N:], '--', color='r', label="CFHTLenS")
plt.plot(x, theory[N:], '--', color='orange', label="Sheth and Tormen")
plt.plot(x, upper_percentile1[N:], color='g', alpha=0.5,
         label="Upper value {0} %" .format(100-lower1))
plt.plot(x, lower_percentile1[N:], color='g', alpha=0.5)
plt.plot(x, upper_percentile2[N:], color='b', alpha=0.5,
         label="Upper value {0} %" .format(100-lower2))
plt.plot(x, lower_percentile2[N:], color='b', alpha=0.5)
plt.legend()

plt.errorbar(x, xim, np.sqrt(errm), fmt='.k',  elinewidth=0.5, capsize=3)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_-$')
plt.savefig('figures/CFHT/xim_percentile.png', bbox_inches='tight')

if not others:
    plt.show()
    quit()
print("Creating {0} sampled plots" .format(nbr))
# plot many
plt.figure(5).set_size_inches((8, 8), forward=False)
plt.title(
    "Correlation function $\\xi_+$")
plt.plot(x, model[:N], label="CFHTLenS")
plt.plot(x, theory[:N], '--', label="Sheth and Tormen")
plt.legend()
plt.errorbar(x, xip, np.sqrt(errp), fmt='.k',  elinewidth=0.5, capsize=3)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_+$')


plt.figure(6).set_size_inches((8, 8), forward=False)
plt.title(
    "Correlation function $\\xi_-$")
plt.plot(x, model[N:], label="CFHTLenS")
plt.plot(x, theory[N:], '--', label="Sheth and Tormen")
plt.legend()
plt.errorbar(x, xim, np.sqrt(errm), fmt='.k',  elinewidth=0.5, capsize=3)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_-$')

for q, p in samples[np.random.randint(len(samples), size=nbr)]:
    values = realMf.CFHTv(N, q, p)
    plt.figure(5)
    plt.plot(x, values[:N], color="k", alpha=0.03)
    plt.figure(6)
    plt.plot(x, values[N:], color="k", alpha=0.03)


plt.figure(5)
plt.savefig('figures/CFHT/xip_var.png', bbox_inches='tight')
plt.figure(6)
plt.savefig('figures/CFHT/xim_var.png', bbox_inches='tight')


plt.show(fig)
# plt.show()
