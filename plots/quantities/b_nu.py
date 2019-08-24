import matplotlib.pyplot as plt
import numpy as np
import os
import sys

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

dc = 1.686
q = 0.707
p = 0.3


def b_st(nu):
    return (1.+(q*(nu**2)-1.+2.*p/(1.+(q*nu**2)**p))/dc)


nu = np.linspace(0.01, 5, 1000)
b_nu = b_st(nu)

plt.figure(1).set_size_inches((8, 8), forward=False)
plt.plot(nu, b_nu)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('$\\nu$')
plt.ylabel('$b(\\nu)$')
plt.title("Halo bias")
os.system('mkdir -p figures/quantities')
plt.savefig('figures/quantities/b_nu.png', dpi=200, bbox_inches='tight')
plt.show()
