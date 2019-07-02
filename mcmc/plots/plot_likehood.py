import corner
import emcee
import scipy.optimize as op
import numpy as np
import matplotlib.pyplot as plt
import realMf
import os
import sys
# model parameters
p_st = 0.3
q_st = 0.707

# other start
p_o = 0.2
q_o = 0.6


# converting to log values
p_st_log = - np.log(p_st)
q_st_log = - np.log(q_st)


p_o_log = - np.log(p_o)
q_o_log = - np.log(q_o)

# MCMC parameters
ndim, nwalkers, steps, firsts = 2, 100, 250, 50

# Importing Data from CFHT
print("Loading data")
N = 21
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


def lnlike(theta_log, nth, y, invcov, par):
    q_log, p_log = theta_log
    # q = np.exp(-q_log)
    # p = np.exp(-p_log)
    q = q_log
    p = p_log
    if not (0.5 < q < 1.0 and 0.1 < p < 0.45):
        return -np.inf
    model = realMf.CFHTvPlot(nth, q, p, parallel=par)
    disk = 1
    while model[0] == np.pi:
        model = realMf.CFHTvPlot(nth, q, p, parallel=par)
        print("probem on disk, iteration : {0}" .format(disk))
        disk += 1
    if model[0] == -np.pi:
        return -np.inf
    return -0.5*np.matmul(np.transpose(y-model), np.matmul(invcov, (y-model)))


# fig = plt.figure()
# ax = fig.add_subplot(111, projection='3d')
p = np.arange(0.1, 0.45, 0.01)
q = np.arange(0.5, 0.95, 0.05)

zs = np.zeros((len(p), len(q)))
tot = len(q)*len(p)
it = 1
print("{0} iterations needed" .format(tot))

os.system('rm data/data_*.dat')

for i in range(len(p)):
    for j in range(len(q)):
        zs[i][j] = lnlike([q[j], p[i]], 21, y, yerrinv, False)
        print("iteration {0}/{1}" .format(*[it, tot]))
        it += 1

# P, Q = np.meshgrid(p, q)
# # zs = np.array(lnlike(np.ravel(P), np.ravel(Q)))
# Z = zs.reshape(P.shape)

# ax.plot_surface(P, Q Z)

# ax.set_xlabel('p')
# ax.set_ylabel('q')
# ax.set_zlabel('lnlike')

# plt.show()
