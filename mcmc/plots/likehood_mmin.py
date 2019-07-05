import numpy as np
import sys
import mcmc.correlation as correlation
import CFHTLenS.get as data
import os
import matplotlib.pyplot as plt
import scipy.optimize as op

computes = False
icosmo = int(sys.argv[1])
icosmos = [1, 4, 42]
index = icosmos.index(icosmo)
cosmos = ['Boring', 'WMAP9', 'Planck 2018']
ihm = 3
mmin_st = 14.0

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


nll = lambda *args: -lnlike(*args)
# Best to use log-parameters I think, thanks to the living spaces of p and q
result = op.minimize(nll, [mmin_st],
                     args=(y, yerrinv, True), method='Nelder-Mead', tol=1e-6)
mmin_ml = result["x"][0]
value = lnlike([mmin_ml], y, yerrinv, verbose=True)

print("Best fit is mmin={0}" .format(mmin_ml))


plt.plot(mmins, likes)
plt.plot([mmin_ml], [value], '-ro', label="{0}" .format(mmin_ml))
plt.xlabel('log(mmin)')
plt.ylabel('ln_like')
# plt.yscale('log')
plt.title('Best fit for {0} cosmology' .format(cosmos[index]))
plt.legend()
plt.savefig('mcmc/figures/CFHT/likehood/mmin{0}.png' .format(icosmo))
# plt.show()
