import numpy as np
import sys
import mcmc.correlation as correlation
import CFHTLenS.get as data
import os
import matplotlib.pyplot as plt

computes = False
icosmo = int(sys.argv[1])
ihm = 3
alpha_st = 1.0

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
    alpha = param[0]
    if not (0.5 <= alpha <= 1.5):
        return -np.inf
    model = correlation.xi_ampCFHT(alpha, icosmo, ihm, verbose=verbose)
    if model[0] == -np.pi:
        return -np.inf
    return -0.5*np.matmul(np.transpose(y-model), np.matmul(invcov, (y-model)))


os.system("mkdir -p mcmc/figures/CFHT/likehood")
os.system("mkdir -p mcmc/data")
alphas = np.linspace(0.5, 1.5, 100)

if computes:
    likes = np.array([lnlike([alpha], y, yerrinv, verbose=True) for alpha in alphas])
    np.save('mcmc/data/likehood_alpha{0}.npy' .format(icosmo), likes)
else:
    likes = np.load('mcmc/data/likehood_alpha{0}.npy' .format(icosmo))

plt.plot(alphas, likes)
plt.xlabel('log(alpha)')
plt.ylabel('ln_like')
plt.savefig('mcmc/figures/CFHT/likehood/alpha{0}.png' .format(icosmo))
plt.show()
