import corner
import emcee
import scipy.optimize as op
import numpy as np
import matplotlib.pyplot as plt
import correlation
import CFHTLenS.get as dataCFHT
import os
import sys
import pygtc


# code mode
if len(sys.argv) != 4:
    print("Expecting 3 parameters : icosmo , data in ['CFHT', 'KiDs'], ihm")
    quit()

icosmo = int(sys.argv[1])
ihm = int(sys.argv[3])
usedData = sys.argv[2]

# MCMC parameters
ndim, nwalkers, steps, firsts = 2, 200, 1000, 150
nbr = 100
others = False
percentiles = False
lower1 = 10
lower2 = 25
print("Loading and computing data")

data = open("mcmc/results/{1}/ihm={2}/fit{0}.txt" .format(*[icosmo, usedData, ihm]), "r")
line = data.readlines()
data.close()

values = line[0].split(',')

q_ml = float(values[0].split('=')[1])
p_ml = float(values[1].split('=')[1])

if usedData == 'CFHT':
    N = 21
else:
    print("Error, choose between CFHT and KiDs data")
    quit()

q_st = 0.707
p_st = 0.3

x = dataCFHT.thetas()

# Data from CFHTLenS survey
xip = dataCFHT.xip()
xim = dataCFHT.xim()
y = xip.copy()
y = np.append(y, xim)

# Considering the real covariance matrix and all kind of errors
yerr = dataCFHT.cov_mat()
yerrinv = np.linalg.inv(yerr)
det = np.linalg.det(yerr)
errp = dataCFHT.sigp()
errm = dataCFHT.sigm()

chain = np.load('mcmc/results/{1}/ihm={2}/chain_st{0}.npy' .format(*[icosmo, usedData, ihm]))

otherchain = chain.reshape((nwalkers, steps, ndim))

# removing first steps
samples = chain[:, firsts:, :].reshape((-1, ndim))

# plot model with results

os.system("mkdir -p mcmc/figures/{0}/ihm={1}/" .format(*[usedData, ihm]))

if usedData == 'CFHT':
    model = correlation.xiCFHT(q_ml, p_ml, icosmo, ihm, True)
    theory = correlation.xiCFHT(q_st, p_st, icosmo, ihm, True)

print("Plotting basic results")
plt.figure(1).set_size_inches((8, 8), forward=False)
plt.title(
    "Correlation function $\\xi_+$")
plt.plot(x, model[:N], label="{0}" .format(usedData))
plt.plot(x, theory[:N], '--', label="Sheth and Tormen")
plt.legend()
plt.errorbar(x, xip, errp, fmt='.k',  elinewidth=0.5, capsize=3)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_+$')


plt.savefig('mcmc/figures/{1}/ihm={2}/xip{0}.png' .format(*[icosmo, usedData, ihm]), bbox_inches='tight', dpi=200)
plt.figure(2).set_size_inches((8, 8), forward=False)
plt.title(
    "Correlation function $\\xi_-$")
plt.plot(x, model[N:], label="{0}" .format(usedData))
plt.plot(x, theory[N:], '--', label="Sheth and Tormen")
plt.legend()

plt.errorbar(x, xim, errm, fmt='.k',  elinewidth=0.5, capsize=3)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_-$')
plt.savefig('mcmc/figures/{1}/ihm={2}/xim{0}.png' .format(*[icosmo, usedData, ihm]), bbox_inches='tight', dpi=200)

# plots of the sampler
plt.figure(3)
for i in range(nwalkers):
    ax1 = plt.subplot(311)
    ax1.plot(chain[i, :, 0], color='black')
    ax2 = plt.subplot(312, sharex=ax1)
    ax2.plot(chain[i, :, 1], color='black')

plt.figure(3)
plt.savefig("mcmc/figures/{1}/ihm={2}/mcmc_walkers{0}.png" .format(*[icosmo, usedData, ihm]), dpi=200)

# fig = corner.corner(samples, labels=["$q$", "$p$"], truths=[q_ml, p_ml])
fig = pygtc.plotGTC(chains=[samples], paramNames=["$q$", "$p$"], truths=[(q_ml, p_ml), (q_st, p_st)], truthLabels=('CFHT', 'Sheth and Tormen'))
fig.set_size_inches((8, 8), forward=False)
fig.savefig("mcmc/figures/{1}/ihm={2}/mcmc_contours{0}.png" .format(*[icosmo, usedData, ihm]), dpi=200)

print("Basic plots created")

if percentiles:
    print("Computing and ploting percentiles")
    q_mcmc1, p_mcmc1 = map(lambda v: (
        v[1], v[2]-v[1], v[1]-v[0]), zip(*np.percentile(samples, [lower1, 50, 100-lower1], axis=0)))
    print("MCMC found these values for {0}/100 certainties" .format(lower1))
    print("q={0} + {1}, - {2}" .format(*[q_mcmc1[0], q_mcmc1[1], q_mcmc1[2]]))
    print("p={0} + {1}, - {2}" .format(*[p_mcmc1[0], p_mcmc1[1], p_mcmc1[2]]))

    # Save these values
    data = open("mcmc/results/{1}/ihm={2}/mcmc{0}.txt" .format(*[icosmo, usedData, ihm]), "w")
    data.write("MCMC found these values for {0}/100 certainties" .format(lower1))
    data.write("\nq={0} + {1}, - {2}" .format(*[q_mcmc1[0], q_mcmc1[1], q_mcmc1[2]]))
    data.write("\np={0} + {1}, - {2}" .format(*[p_mcmc1[0], p_mcmc1[1], p_mcmc1[2]]))

    q_mcmc2, p_mcmc2 = map(lambda v: (
        v[1], v[2]-v[1], v[1]-v[0]), zip(*np.percentile(samples, [lower2, 50, 100-lower2], axis=0)))
    print("MCMC found these values for {0}/100 certainties" .format(lower2))
    print("q={0} + {1}, - {2}" .format(*[q_mcmc2[0], q_mcmc2[1], q_mcmc2[2]]))
    print("p={0} + {1}, - {2}" .format(*[p_mcmc2[0], p_mcmc2[1], p_mcmc2[2]]))

    data.write("\nMCMC found these values for {0}/100 certainties" .format(lower2))
    data.write("\nq={0} + {1}, - {2}" .format(*[q_mcmc2[0], q_mcmc2[1], q_mcmc2[2]]))
    data.write("\np={0} + {1}, - {2}" .format(*[p_mcmc2[0], p_mcmc2[1], p_mcmc2[2]]))
    data.close()

    # Compute contours
    upper_percentile1 = correlation.xiCFHT(q_mcmc1[0]+q_mcmc1[1], p_mcmc1[0]+p_mcmc1[1], icosmo, ihm, True)
    lower_percentile1 = correlation.xiCFHT(q_mcmc1[0]-q_mcmc1[2], p_mcmc1[0]-p_mcmc1[2], icosmo, ihm, True)

    upper_percentile2 = correlation.xiCFHT(q_mcmc2[0]+q_mcmc2[1], p_mcmc2[0]+p_mcmc2[1], icosmo, ihm, True)
    lower_percentile2 = correlation.xiCFHT(q_mcmc2[0]-q_mcmc2[2], p_mcmc2[0]-p_mcmc2[2], icosmo, ihm, True)

    plt.figure(7).set_size_inches((8, 8), forward=False)
    plt.title(
        "Correlation function $\\xi_+$")
    plt.plot(x, model[:N], '--', color='r', label="{0}" .format(usedData))
    plt.plot(x, theory[:N], '--', color='orange', label="Sheth and Tormen")
    plt.plot(x, upper_percentile1[:N], color='g', alpha=0.5,
             label="Upper value {0} %" .format(100-lower1))
    plt.plot(x, lower_percentile1[:N], color='g', alpha=0.5)
    plt.plot(x, upper_percentile2[:N], color='b', alpha=0.5,
             label="Border value {0} %" .format(100-lower2))
    plt.plot(x, lower_percentile2[:N], color='b', alpha=0.5)
    plt.legend()

    plt.errorbar(x, xip, errp, fmt='.k',  elinewidth=0.5, capsize=3)
    plt.xscale('log')
    plt.yscale('log')
    plt.xlabel('$\\theta (arcmin)$')
    plt.ylabel('$\\xi_+$')
    plt.savefig('mcmc/figures/{1}/ihm={2}/xip_percentile{0}.png' .format(*[icosmo, usedData, ihm]), bbox_inches='tight', dpi=200)

    plt.figure(8).set_size_inches((8, 8), forward=False)
    plt.title(
        "Correlation function $\\xi_-$")
    plt.plot(x, model[N:], '--', color='r', label="{0}" .format(usedData))
    plt.plot(x, theory[N:], '--', color='orange', label="Sheth and Tormen")
    plt.plot(x, upper_percentile1[N:], color='g', alpha=0.5,
             label="Upper value {0} %" .format(100-lower1))
    plt.plot(x, lower_percentile1[N:], color='g', alpha=0.5)
    plt.plot(x, upper_percentile2[N:], color='b', alpha=0.5,
             label="Upper value {0} %" .format(100-lower2))
    plt.plot(x, lower_percentile2[N:], color='b', alpha=0.5)
    plt.legend()

    plt.errorbar(x, xim, errm, fmt='.k',  elinewidth=0.5, capsize=3)
    plt.xscale('log')
    plt.yscale('log')
    plt.xlabel('$\\theta (arcmin)$')
    plt.ylabel('$\\xi_-$')
    plt.savefig('mcmc/figures/{1}/ihm={2}/xim_percentile{0}.png' .format(*[icosmo, usedData, ihm]), bbox_inches='tight', dpi=200)

if not others:
    # plt.show()
    quit()

print("Creating {0} sampled plots" .format(nbr))
# plot many
plt.figure(5).set_size_inches((8, 8), forward=False)
plt.title(
    "Correlation function $\\xi_+$")
plt.plot(x, model[:N], label="{0}" .format(usedData))
plt.plot(x, theory[:N], '--', label="Sheth and Tormen")
plt.legend()
plt.errorbar(x, xip, errp, fmt='.k',  elinewidth=0.5, capsize=3)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_+$')


plt.figure(6).set_size_inches((8, 8), forward=False)
plt.title(
    "Correlation function $\\xi_-$")
plt.plot(x, model[N:], label="{0}" .format(usedData))
plt.plot(x, theory[N:], '--', label="Sheth and Tormen")
plt.legend()
plt.errorbar(x, xim, errm, fmt='.k',  elinewidth=0.5, capsize=3)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_-$')

for q, p in samples[np.random.randint(len(samples), size=nbr)]:
    values = correlation.xiCFHT(q, p, icosmo, ihm, True)
    plt.figure(5)
    plt.plot(x, values[:N], color="k", alpha=0.03)
    plt.figure(6)
    plt.plot(x, values[N:], color="k", alpha=0.03)


plt.figure(5)
plt.savefig('mcmc/figures/{1}/ihm={2}/xip_var{0}.png' .format(*[icosmo, usedData, ihm]), bbox_inches='tight', dpi=200)
plt.figure(6)
plt.savefig('mcmc/figures/{1}/ihm={2}/xim_var{0}.png' .format(*[icosmo, usedData, ihm]), bbox_inches='tight', dpi=200)


# plt.show(fig)
# plt.show()
