import numpy as np
import sys
import mcmc.correlation as correlation
import CFHTLenS.get as data
import os
import matplotlib.pyplot as plt
import scipy.optimize as op

computes = True
sig8_st = 0.8
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
    sig8 = param[0]
    if not (0.1 <= sig8 <= 0.9):
        return -np.inf
    model = correlation.xi_sig8CFHT(sig8, verbose=verbose)
    if model[0] == -np.pi:
        return -np.inf
    return -0.5*np.matmul(np.transpose(y-model), np.matmul(invcov, (y-model)))


os.system("mkdir -p mcmc/figures/CFHT/likehood")
os.system("mkdir -p mcmc/data")
sig8s = np.linspace(0.1, 0.9, 100)

if computes:
    likes = np.array([lnlike([sig8], y, yerrinv, verbose=True) for sig8 in sig8s])
    np.save('mcmc/data/likehood_sig8.npy', likes)
else:
    likes = np.load('mcmc/data/likehood_sig8.npy')


nll = lambda *args: -lnlike(*args)
# Best to use log-parameters I think, thanks to the living spaces of p and q
result = op.minimize(nll, [sig8_st],
                     args=(y, yerrinv, True), method='Nelder-Mead', tol=1e-6)
sig8_ml = result["x"][0]
value = lnlike([sig8_ml], y, yerrinv, verbose=True)

print("Best fit is sig8={0}" .format(sig8_ml))


plt.plot(sig8s, likes)
plt.plot([sig8_ml], [value], '-ro', label="{0}" .format(sig8_ml))
plt.xlabel('$\\sigma_8$')
plt.ylabel('ln_like')
plt.legend()
plt.savefig('mcmc/figures/CFHT/likehood/sig8.png')
plt.show()
