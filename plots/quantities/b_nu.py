import matplotlib.pyplot as plt
import numpy as np
import os
import sys

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
