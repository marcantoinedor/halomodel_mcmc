import emcee
import scipy.optimize as op
import numpy as np
import matplotlib.pyplot as plt
import correlation
import CFHTLenS.get as dataCFHT
import os
import sys
import pygtc

if len(sys.argv) != 2:
    print("Expecting 1 argument : ihm")
    quit()

ihm = int(sys.argv[1])

usedData = 'CFHT'
icosmos = [4, 42]
cosmos = ['WMAP9', 'Planck 2018']
index_cosmo = 0
names = ['$q$', '$p$']
# MCMC parameters
ndim, nwalkers, steps, firsts = 2, 200, 1000, 150


print("Loading and computing data")
samples = []
chainLabels = []
truths = []
truthLabels = []

for icosmo in icosmos:

    data = open("mcmc/results/{1}/ihm={2}/fit{0}.txt" .format(*[icosmo, usedData, ihm]), "r")
    line = data.readlines()
    data.close()

    values = line[0].split(',')

    q_ml = float(values[0].split('=')[1])
    p_ml = float(values[1].split('=')[1])

    truths.append((q_ml, p_ml))
    truthLabels.append(cosmos[index_cosmo])
    chain = np.load('mcmc/results/{1}/ihm={2}/chain_st{0}.npy' .format(*[icosmo, usedData, ihm]))

    # removing first steps and adding to samples list
    samples.append(chain[:, firsts:, :].reshape((-1, ndim)))
    chainLabels.append(cosmos[index_cosmo])
    index_cosmo += 1

q_st = 0.707
p_st = 0.3

truths.append((q_st, p_st))
truthLabels.append('Sheth and Tormen')

truthColors = ['blue', 'red', 'green', 'purple'][:len(truths)]
fig = pygtc.plotGTC(chains=samples, paramNames=names, chainLabels=chainLabels, truths=truths, truthLabels=truthLabels, truthColors=truthColors, nContourLevels=3, sigmaContourLevels=True)
fig.set_size_inches((8, 8), forward=False)
fig.savefig("mcmc/figures/{0}/ihm={1}/mcmc_contours_cosmos.png" .format(*[usedData, ihm]), dpi=200)
plt.show()
