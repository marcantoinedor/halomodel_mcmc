import matplotlib.pyplot as plt
import os
import sys
from colour import Color
import numpy as np
import CFHTLenS.get as get
import utils.create_data as create
import utils.get_correlation as dat

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 15

plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

icosmos = [1, 4, 42, 50]
cosmos = ['Fiducial', 'WMAP9', 'Planck 2018', 'Kilbinger']

clean = False
if len(sys.argv) == 2:
    clean = (sys.argv[1] == 'clean')

CFHT_data = True

thetasCFHT = get.thetas()
xipCFHT = get.xip()
ximCFHT = get.xim()
sigmCFHT = get.sigm()
sigpCFHT = get.sigp()


begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(icosmos)))

create.xi_CFHT_cosmo(icosmos, clean=clean)

x_axis = dat.get_x_axis_cosmo()

index = 0
for icosmo in icosmos:

    xip = dat.get_xip_cosmo(icosmo)
    xim = dat.get_xim_cosmo(icosmo)

    plt.figure(1)
    plt.plot(x_axis, xip, color=colors[index].rgb, label="{0}" .format(cosmos[index]))
    plt.figure(3)
    plt.plot(x_axis, xim, color=colors[index].rgb, label="{0}" .format(cosmos[index]))
    index += 1

os.system("mkdir -p figures/icosmo/")

plt.figure(1).set_size_inches((8, 8), forward=False)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_{+}$')
plt.errorbar(x_axis, xipCFHT, sigpCFHT, fmt='.k', elinewidth=0.5, capsize=3)
plt.legend()
plt.savefig('figures/icosmo/xip', dpi=200, bbox_inches='tight')

plt.figure(3).set_size_inches((8, 8), forward=False)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_{-}$')
plt.errorbar(x_axis, ximCFHT, sigmCFHT, fmt='.k', elinewidth=0.5, capsize=3)
plt.legend()
plt.savefig('figures/icosmo/xim', dpi=200, bbox_inches='tight')

plt.show()
