import matplotlib.pyplot as plt
import os
import sys
from colour import Color
import CFHTLenS.get as get
import utils.create_data as create
import utils.get_correlation as dat

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 15

plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

l_max = 100000
ihms = [1, 3]

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
colors = list(begin_color.range_to(Color("green"), len(ihms)))

create.xi_CFHT_ihm(ihms, l_max, clean=clean)

os.system('mkdir -p figures/correlation/')

plt.figure(1).set_size_inches((8, 8), forward=False)
plt.figure(3).set_size_inches((8, 8), forward=False)

# Get x_axis
x_axis = dat.get_x_axis_ihm()

i = 0
for ihm in ihms:

    column1 = dat.get_xip_ihm(ihm)
    column3 = dat.get_xim_ihm(ihm)

    plt.figure(1)
    plt.plot(x_axis, column1, color=colors[i].rgb, label="ihm={0}" .format(ihm))
    plt.figure(3)
    plt.plot(x_axis, column3, color=colors[i].rgb, label="ihm={0}" .format(ihm))
    i += 1

plt.figure(1)
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_{+}$')
plt.xscale('log')
plt.yscale('log')
plt.title("Correlation function $\\xi_+$")
plt.legend()
if CFHT_data:
    plt.errorbar(thetasCFHT, xipCFHT, sigpCFHT, fmt='.k', elinewidth=0.5, capsize=3)
    plt.savefig('figures/correlation/xip_CFHT_ihm.png', dpi=200, bbox_inches='tight')
else:
    plt.savefig('figures/correlation/xip_ihm.png', dpi=200, bbox_inches='tight')

plt.figure(3)
plt.xlabel('$\\theta (arcmin)$')
plt.ylabel('$\\xi_{-}$')
plt.xscale('log')
plt.yscale('log')
plt.title("Correlation function $\\xi_-$")
plt.legend()
if CFHT_data:
    plt.errorbar(thetasCFHT, ximCFHT, sigmCFHT, fmt='.k', elinewidth=0.5, capsize=3)
    plt.savefig('figures/correlation/xim_CFHT_ihm.png', dpi=200, bbox_inches='tight')
else:
    plt.savefig('figures/correlation/xim_ihm.png', dpi=200, bbox_inches='tight')
plt.show()
