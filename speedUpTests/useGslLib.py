import pyfftlog as ht
import get
import os
from colour import Color
import numpy as np
import matplotlib.pyplot as plt
import sys
sys.path.append('/home/marco/pyfftlog')

ref = 1000000

data = open("data/lmax=1000000.0/xi1.dat", "r")
lines = data.readlines()
data.close()
refs1 = [float(val) for val in lines]

data = open("data/lmax=1000000.0/xi3.dat", "r")
lines = data.readlines()
data.close()
refs3 = [float(val) for val in lines]

os.system('mkdir -p data/hankel')
os.system('./bin/halo_model_hankel')

data = open("data/hankel/l.dat", "r")
lines = data.readlines()
data.close()
ls = [float(val) for val in lines]
dlnr = np.log(ls[1])-np.log(ls[0])

# ls = np.linspace(np.log(lmin), np.log(lmax), nl)
# dlnr = ls[1]-ls[0]
# ls = np.exp(ls)

data = open("data/hankel/pow2D.dat", "r")
lines = data.readlines()
data.close()
refs = [float(val) for val in lines]
refs = np.array(refs)
thsarcmin = np.array(get.get_theta_CFHT())
# tharcmin = np.array(tharcmin)
thsdeg = thsarcmin/60
nth = thsarcmin.size

# Send numpy arrays
kr, xsave = ht.fhti(refs.size, 0, dlnr, 0, 1, 0)
xip = ht.fht(refs, xsave)
print(xip)
print(xip.size)

# xim = ht.fht(refs, tharcmin)


plt.figure(1).set_size_inches((8, 8), forward=False)
# plt.plot(thsarcmin, [abs(xip[i]*2*np.pi - refs1[i])/refs1[i] for i in range(nth)])
plt.plot(refs)
plt.plot(xip)

plt.title("Variation of $\\xi_+$ changing modes number, lref={0}" .format(ref))
plt.xlabel("$\\theta$ (arcmin)")
plt.ylabel("$(\\xi_+-\\xi_m)/ \\xi_m$")
plt.yscale('log')
plt.xscale('log')
# plt.legend()
# plt.savefig('figures/hankel1.png', bbox_inches='tight')

# plt.legend(bbox_to_anchor=(0.8, 1.25), loc='lower right')


# plt.figure(3).set_size_inches((8, 8), forward=False)
# plt.plot(thsarcmin, [abs(xim[i]*2*np.pi - refs3[i])/refs3[i] for i in range(nth)])
# plt.title("Variation of $\\xi_-$ changing modes number, lref={0}" .format(ref))
# plt.xlabel("$\\theta$ (arcmin)")
# plt.ylabel("$(\\xi_--\\xi_m)/ \\xi_m$")
# plt.yscale('log')
# plt.xscale('log')
# # plt.legend()
# plt.savefig('figures/hankel3.png', bbox_inches='tight')

# # plt.legend(bbox_to_anchor=(0.8, 1.25), loc='lower right')

plt.figure(11).set_size_inches((8, 8), forward=False)
# plt.plot(thsarcmin, refs1, label='Fortran')
# plt.plot(thsarcmin, xim, label='Python')
plt.xlabel('$\\theta$ (arcmin)')
plt.ylabel('$\\xi_-$')
plt.xscale('log')
plt.yscale('log')
plt.legend()
# plt.savefig('figures/hankel1comp.png')

# plt.figure(33).set_size_inches((8, 8), forward=False)
# plt.plot(thsarcmin, refs3, label='Fortran')
# plt.plot(thsarcmin, xip, label='Python')
# plt.xlabel('$\\theta$ (arcmin)')
# plt.ylabel('$\\xi_+$')
# plt.xscale('log')
# plt.yscale('log')
# plt.legend()
# plt.savefig('figures/hankel3comp.png')

plt.show()
