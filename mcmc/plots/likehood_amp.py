import numpy as np
import sys
import mcmc.correlation as correlation
import CFHTLenS.get as data
import os
import matplotlib.pyplot as plt
import scipy.optimize as op

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

computes = False
icosmo = int(sys.argv[1])
icosmos = [1, 4, 42]
index = icosmos.index(icosmo)
cosmos = ['Fiducial', 'WMAP9', 'Planck 2018']
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


os.system("mkdir -p mcmc/figures/CFHT/likehood/ihm={0}" .format(ihm))
os.system("mkdir -p mcmc/data/ihm={0}" .format(ihm))

alphas = np.linspace(0.5, 1.5, 100)

if computes or (not os.path.isfile('mcmc/data/ihm={1}/likehood_alpha{0}.npy' .format(*[icosmo, ihm]))):
    likes = np.array([lnlike([alpha], y, yerrinv, verbose=True) for alpha in alphas])
    np.save('mcmc/data/ihm={1}/likehood_alpha{0}.npy' .format(*[icosmo, ihm]), likes)
else:
    likes = np.load('mcmc/data/ihm={1}/likehood_alpha{0}.npy' .format(*[icosmo, ihm]))


nll = lambda *args: -lnlike(*args)
# Best to use log-parameters I think, thanks to the living spaces of p and q
result = op.minimize(nll, [alpha_st],
                     args=(y, yerrinv, True), method='Nelder-Mead', tol=1e-6)
alpha_ml = result["x"][0]
value = lnlike([alpha_ml], y, yerrinv, verbose=True)

print("Best fit is alpha={0}" .format(alpha_ml))

plt.figure().set_size_inches((8, 8), forward=False)
plt.plot(alphas, likes)
plt.plot([alpha_ml], [value], '-ro', label="{0}" .format(alpha_ml))
plt.title('Best fit for {0} cosmology' .format(cosmos[index]))
plt.xlabel('$\\alpha$')
plt.ylabel('ln_like')
plt.legend()
plt.savefig('mcmc/figures/CFHT/likehood/ihm={1}/alpha{0}.png' .format(*[icosmo, ihm]))
plt.show()
