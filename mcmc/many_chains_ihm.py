import scipy.optimize as op
import numpy as np
import matplotlib.pyplot as plt
import os
import sys
import pygtc

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

if len(sys.argv) != 2:
    print("Expecting 1 argument : icosmo")
    quit()

icosmo = int(sys.argv[1])

usedData = 'CFHT'
ihms = [1, 3]
halomodels = ['HMcode', 'Halo-Model']
index_ihm = 0
names = ['$q$', '$p$']
# MCMC parameters
ndim, nwalkers, steps, firsts = 2, 200, 1000, 150


print("Loading and computing data")
samples = []
chainLabels = []
truths = []
truthLabels = []

for ihm in ihms:

    data = open("mcmc/results/{1}/ihm={2}/fit{0}.txt" .format(*[icosmo, usedData, ihm]), "r")
    line = data.readlines()
    data.close()

    values = line[0].split(',')

    q_ml = float(values[0].split('=')[1])
    p_ml = float(values[1].split('=')[1])

    truths.append((q_ml, p_ml))
    truthLabels.append(halomodels[index_ihm])
    chain = np.load('mcmc/results/{1}/ihm={2}/chain_st{0}.npy' .format(*[icosmo, usedData, ihm]))

    # removing first steps and adding to samples list
    samples.append(chain[:, firsts:, :].reshape((-1, ndim)))
    chainLabels.append(halomodels[index_ihm])
    index_ihm += 1

q_st = 0.707
p_st = 0.3

truths.append((q_st, p_st))
truthLabels.append('Sheth and Tormen')

truthColors = ['blue', 'red', 'green', 'purple'][:len(truths)]
fig = pygtc.plotGTC(chains=samples, paramNames=names, chainLabels=chainLabels, truths=truths, truthLabels=truthLabels,
                    truthColors=truthColors, nContourLevels=3, sigmaContourLevels=True, figureSize="MNRAS_page")
fig.set_size_inches((8, 8), forward=False)
os.system("mkdir -p mcmc/figures/{0}/icosmo={1}" .format(*[usedData, icosmo]))
fig.savefig("mcmc/figures/{0}/icosmo={1}/mcmc_contours_ihms.png" .format(*[usedData, icosmo]), dpi=200)
plt.show()
