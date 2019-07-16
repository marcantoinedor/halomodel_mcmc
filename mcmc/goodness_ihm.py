import numpy as np
import sys
import correlation
import CFHTLenS.get as dat
import os

icosmos = [4, 42, 50]
cosmos = ['WMAP9', 'Planck 2018', 'Kilbinger']
index_cosmo = 0
ihms = [1, 3]
halo_models = ['HMCode', 'Halomodel']
index_ihm = 0

usedData = 'CFHT'

# Importing Data from CFHT
print("Loading data")

# Length of CFHT thetas data
N = 21
x = dat.thetas()

# Data from CFHTLenS survey
xip = dat.xip()
xim = dat.xim()
y = xip.copy()
y = np.append(y, xim)

# Considering the real covariance matrix and all kind of errors
yerr = dat.cov_mat()
yerrinv = np.linalg.inv(yerr)
det = np.linalg.det(yerr)
errp = dat.sigp()
errm = dat.sigm()

for ihm in ihms:

    index_cosmo = 0
    for icosmo in icosmos:

        def khisquare(params, y, invcov, verbose=False):
            q, p = params
            if not (0.2 < q < 1.5 and 0.0 <= p < 0.5):
                return np.inf
            model = correlation.xiCFHT(q, p, icosmo, ihm, verbose=verbose)
            if model[0] == -np.pi:
                return np.inf
            return np.matmul(np.transpose(y-model), np.matmul(invcov, (y-model)))

        # computation of khi square values for all best fits
        data = open("mcmc/results/{1}/ihm={2}/fit{0}.txt" .format(*[icosmo, usedData, ihm]), "r")
        line = data.readlines()
        data.close()

        values = line[0].split(',')

        q_ml = float(values[0].split('=')[1])
        p_ml = float(values[1].split('=')[1])

        q_st = 0.707
        p_st = 0.3

        # Degrees of freedom
        d = N-2
        ki2_ml = khisquare([q_ml, p_ml], y, yerrinv, verbose=True)
        reduced_ki2_ml = ki2_ml/d
        print("{3}, {2} cosmology : chi^2={0}, reduced is chi^2_r={1}" .format(*[ki2_ml, reduced_ki2_ml, cosmos[index_cosmo], halo_models[index_ihm]]))
        index_cosmo += 1
    index_ihm += 1

# High reduced khi lead to a bad model, low reduced khi is a bad data in errorbars
