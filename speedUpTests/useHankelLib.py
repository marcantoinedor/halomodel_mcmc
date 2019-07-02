import matplotlib.pyplot as plt
import numpy as np
from colour import Color
import os
from hankel import HankelTransform
from scipy.interpolate import interp1d
from scipy.interpolate import InterpolatedUnivariateSpline as Spline


ref = 1000000

data = open("data/lmax=1000000.0/xi1.dat", "r")
lines = data.readlines()
data.close()
refs1 = [float(val) for val in lines]

data = open("data/lmax=1000000.0/xi3.dat", "r")
lines = data.readlines()
data.close()
refs3 = [float(val) for val in lines]

lmin = 1e-2
lmax = 1e5
nl = 100

ls = np.linspace(np.log(lmin), np.log(lmax), nl)
ls = np.exp(ls)

# theta in rad
thmin = 0.5*1/60*np.pi/180
thmax = 100*1/60*np.pi/180
nth = 100

ths = np.linspace(np.log(thmin), np.log(thmax), nth)
ths = np.exp(ths)

thsarcmin = 180/np.pi*60*ths
thsdegree = thsarcmin/60

os.system('mkdir -p data/hankel')
# os.system('./bin/halo_model_hankel')

data = open("data/hankel/pow2D.dat", "r")
lines = data.readlines()
data.close()
refs = [float(val) for val in lines]

# Interpolating function for Hankel transform
# P2D = interp1d(ls, refs, kind='cubic')
spl = Spline(np.log(ths), np.log(refs), k=1)


def P2D(x): return np.exp(spl(np.log(x)))


# Xi+
htp = HankelTransform(
    nu=0,     # The order of the bessel function
    N=120000,   # Number of steps in the integration
    h=0.01   # Proxy for "size" of steps in integration
)


xip = htp.transform(P2D, ths, ret_err=False)

# Xi-
htm = HankelTransform(
    nu=4,     # The order of the bessel function
    N=120000,   # Number of steps in the integration
    h=0.01   # Proxy for "size" of steps in integration
)

xim = htm.transform(P2D, ths, ret_err=False)


plt.figure(1).set_size_inches((8, 8), forward=False)
plt.plot(thsarcmin, [abs(xip[i]*2*np.pi - refs1[i])/refs1[i] for i in range(nth)])
plt.title("Variation of $\\xi_+$ changing modes number, lref={0}" .format(ref))
plt.xlabel("$\\theta$ (arcmin)")
plt.ylabel("$(\\xi_+-\\xi_m)/ \\xi_m$")
plt.yscale('log')
plt.xscale('log')
# plt.legend()
plt.savefig('figures/hankel1.png', bbox_inches='tight')

# plt.legend(bbox_to_anchor=(0.8, 1.25), loc='lower right')


plt.figure(3).set_size_inches((8, 8), forward=False)
plt.plot(thsarcmin, [abs(xim[i]*2*np.pi - refs3[i])/refs3[i] for i in range(nth)])
plt.title("Variation of $\\xi_-$ changing modes number, lref={0}" .format(ref))
plt.xlabel("$\\theta$ (arcmin)")
plt.ylabel("$(\\xi_--\\xi_m)/ \\xi_m$")
plt.yscale('log')
plt.xscale('log')
# plt.legend()
plt.savefig('figures/hankel3.png', bbox_inches='tight')

# plt.legend(bbox_to_anchor=(0.8, 1.25), loc='lower right')

plt.figure(11).set_size_inches((8, 8), forward=False)
plt.plot(thsarcmin, refs1, label='Fortran')
plt.plot(thsarcmin, xim, label='Python')
plt.xlabel('$\\theta$ (arcmin)')
plt.ylabel('$\\xi_-$')
plt.xscale('log')
plt.yscale('log')
plt.legend()
plt.savefig('figures/hankel1comp.png')

plt.figure(33).set_size_inches((8, 8), forward=False)
plt.plot(thsarcmin, refs3, label='Fortran')
plt.plot(thsarcmin, xip, label='Python')
plt.xlabel('$\\theta$ (arcmin)')
plt.ylabel('$\\xi_+$')
plt.xscale('log')
plt.yscale('log')
plt.legend()
plt.savefig('figures/hankel3comp.png')

plt.show()
