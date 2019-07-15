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
icosmo = 50
clean = False
if len(sys.argv) == 2:
    clean = (sys.argv[1] == 'clean')

CFHT_data = True

thetasCFHT = get.thetas()
xipCFHT = get.xip()
ximCFHT = get.xim()
sigmCFHT = get.sigm()
sigpCFHT = get.sigp()

create.xi_CFHT_ihm(ihms, l_max, icosmo=icosmo, clean=clean)

os.system('mkdir -p figures/correlation/')


# Get x_axis
x_axis = dat.get_x_axis_ihm()

halo_models = ['Halomodel', 'HMCode']
index_ihm = 0
for ihm in ihms:

    plt.figure().set_size_inches((8, 8), forward=False)
    column1 = dat.get_xip_ihm(ihm)
    column3 = dat.get_xim_ihm(ihm)

    plt.plot(x_axis, column1, color=Color('red').rgb, label="$\\xi_+$")
    plt.plot(x_axis, column3, color=Color('orange').rgb, label="$\\xi_-$")

    plt.xlabel('$\\theta (arcmin)$')
    plt.ylabel('$\\xi(\\theta)$')
    plt.xscale('log')
    plt.yscale('log')
    plt.title("Shear correlation functions, {0}" .format(halo_models[index_ihm]))
    plt.legend()
    if CFHT_data:
        plt.errorbar(thetasCFHT, xipCFHT, sigpCFHT, ecolor=Color('black').rgb, fmt='.k', elinewidth=0.5, capsize=3)
        plt.errorbar(thetasCFHT, ximCFHT, sigmCFHT, ecolor=Color('grey').rgb, fmt='.C7', elinewidth=0.5, capsize=3)

        plt.savefig('figures/correlation/all_CFHT_ihm={0}.png' .format(ihm), dpi=200, bbox_inches='tight')
    else:
        plt.savefig('figures/correlation/all_ihm={0}.png' .format(ihm), dpi=200, bbox_inches='tight')
    index_ihm += 1
plt.show()
