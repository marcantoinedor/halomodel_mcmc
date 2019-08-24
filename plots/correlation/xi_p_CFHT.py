import matplotlib.pyplot as plt
import os
import sys
from colour import Color
import CFHTLenS.get as get
import utils.create_data as create
import utils.get_correlation as dat


SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

qs = [0.6, 0.65, 0.707, 0.75, 0.8, 0.9]
ps = [0., 0.1, 0.2, 0.25, 0.3, 0.35, 0.4]

l_max = 100000

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
colors = list(begin_color.range_to(Color("green"), len(ps)))

for q in qs:

    x_axis = dat.get_x_axis_st()
    i = 0
    for p in ps:
        create.xi_CFHT_st(q, p, l_max, clean=clean)
        column1 = dat.get_xip_st(q, p)
        column3 = dat.get_xim_st(q, p)

        plt.figure(1)
        plt.plot(x_axis, column1, color=colors[i].rgb, label="p={0}" .format(p))
        plt.figure(3)
        plt.plot(x_axis, column3, color=colors[i].rgb, label="p={0}" .format(p))
        i += 1

    os.system('mkdir -p figures/correlation/q={0}' .format(q))

    plt.figure(1).set_size_inches((8, 8), forward=False)
    plt.xlabel('$\\theta (arcmin)$')
    plt.ylabel('$\\xi_{+}$')
    plt.xscale('log')
    plt.yscale('log')
    plt.title("Correlation function varying $p$, $q=${0}" .format(q))
    plt.legend()
    if CFHT_data:
        plt.errorbar(thetasCFHT, xipCFHT, sigpCFHT, fmt='.k', elinewidth=0.5, capsize=3)
        plt.savefig('figures/correlation/q={0}/xip_CFHT.png' .format(q), dpi=200, bbox_inches='tight')
    else:
        plt.savefig('figures/correlation/q={0}/xip.png' .format(q), dpi=200, bbox_inches='tight')
    plt.clf()

    plt.figure(3).set_size_inches((8, 8), forward=False)
    plt.xlabel('$\\theta (arcmin)$')
    plt.ylabel('$\\xi_{-}$')
    plt.xscale('log')
    plt.yscale('log')
    plt.title("Correlation function varying $p$, $q=${0}" .format(q))
    plt.legend()
    if CFHT_data:
        plt.errorbar(thetasCFHT, ximCFHT, sigmCFHT, fmt='.k', elinewidth=0.5, capsize=3)
        plt.savefig('figures/correlation/q={0}/xim_CFHT.png' .format(q), dpi=200, bbox_inches='tight')
    else:
        plt.savefig('figures/correlation/q={0}/xim.png' .format(q), dpi=200, bbox_inches='tight')
    plt.clf()

    print("q={0}" .format(q))
