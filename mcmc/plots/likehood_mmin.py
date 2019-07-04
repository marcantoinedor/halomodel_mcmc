import numpy as np
import sys
import mcmc.correlation as correlation
import CFHTLenS.get as data
import os
import matplotlib.pyplot as plt

computes = False
icosmo = int(sys.argv[1])
ihm = 3
mmin_st = 7.0

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
    if not (5. <= mmin <= 15.):
        return -np.inf
    model = correlation.xi_mminCFHT(mmin, icosmo, ihm, verbose=verbose)
    if model[0] == -np.pi:
        return -np.inf
    return -0.5*np.matmul(np.transpose(y-model), np.matmul(invcov, (y-model)))


os.system("mkdir -p mcmc/figures/CFHT/likehood")
os.system("mkdir -p mcmc/data")
mmins = np.linspace(5.0, 15.0, 100)

if computes:
    likes = np.array([lnlike([mmin], y, yerrinv, verbose=True) for mmin in mmins])
    np.save('mcmc/data/likehood_mmin{0}.npy' .format(icosmo), likes)
else:
    likes = np.load('mcmc/data/likehood_mmin{0}.npy' .format(icosmo))

plt.plot(mmins, likes)
plt.xlabel('log(mmin)')
plt.ylabel('ln_like')
# plt.yscale('log')
plt.savefig('mcmc/figures/CFHT/likehood/mmin{0}.png' .format(icosmo))
plt.show()
