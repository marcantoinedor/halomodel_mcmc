import matplotlib.pyplot as plt
import os
from colour import Color
import numpy as np
import get
SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 15

plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels
# plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
# plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
# plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
# plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title

# qs = [0.707]
# ps = [0.3]

qs = [0.6, 0.65, 0.707, 0.75, 0.8, 0.9]
ps = [0., 0.1, 0.2, 0.25, 0.3, 0.35, 0.4]
l_max = 5000000
clean = False

thetasCFHT = get.get_theta_CFHT()
xipCFHT = get.get_xip_CFHT()
ximCFHT = get.get_xim_CFHT()
sigmCFHT = get.get_sigm_CFHT()
sigpCFHT = get.get_sigp_CFHT()


begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(qs)))

for p in ps:
    for q in qs:
        if clean:
            os.system('mv -f data/q={0}p={1}/xi1_CFHT.dat data/q={0}p={1}/xi1_CFHT_old.dat'.format(*[q, p]))
            os.system('mv -f data/q={0}p={1}/xi3_CFHT.dat data/q={0}p={1}/xi3_CFHT_old.dat'.format(*[q, p]))

        if not os.path.isfile('data/q={0}p={1}/xi1_CFHT.dat'.format(*[q, p])):
            print("Creating data {0}" .format(q))
            os.system('./bin/halo_model_plotxi_CFHT {0} {1} {2}' .format(*[q, p, l_max]))

    os.system('mkdir -p figures/corrFunction/p={0}' .format(p))

    plt.figure(1).set_size_inches((8, 8), forward=False)
    plt.xscale('log')
    plt.xlabel('$\\theta (arcmin)$')
    plt.ylabel('$\\xi_{+}$')
    plt.yscale('log')

    plt.figure(3).set_size_inches((8, 8), forward=False)
    plt.xscale('log')
    plt.xlabel('$\\theta (arcmin)$')
    plt.ylabel('$\\xi_{-}$')
    plt.yscale('log')

    i = 0
    x_axis = []
    data = open('data/q={0}p={1}/xi1_CFHT.dat' .format(*[qs[0], ps[0]]))
    lines = data.readlines()
    data.close()
    for j in range(0, len(lines)):
        value = lines[j].split('  ')[1]
        x_axis.append(float(value.lower()))

    for q in qs:
        data = open('data/q={0}p={1}/xi1_CFHT.dat' .format(*[q, p]))
        lines1 = data.readlines()
        data.close()
        column1 = []

        data = open('data/q={0}p={1}/xi3_CFHT.dat' .format(*[q, p]))
        lines3 = data.readlines()
        data.close()
        column3 = []
        for j in range(0, len(lines)):
            value1 = lines1[j].split('        ')[1]
            column1.append(float(value1.lower()))
            value3 = lines3[j].split('        ')[1]
            column3.append(float(value3.lower()))

        plt.figure(1)
        plt.plot(x_axis, column1, color=colors[i].rgb, label="q={0}" .format(q))
        plt.figure(3)
        plt.plot(x_axis, column3, color=colors[i].rgb, label="q={0}" .format(q))
        i += 1

    plt.figure(1).set_size_inches((8, 8), forward=False)
    plt.title("Correlation function varying $q$, $p=${0}" .format(p))
    plt.errorbar(thetasCFHT, xipCFHT, sigpCFHT, fmt='.k',  elinewidth=0.5, capsize=3)
    plt.legend()
    # os.system('mv figures/corrFunction/p={0}/xip.png figures/corrFunction/p={0}/xip_old.png' .format(p))
    plt.savefig('figures/corrFunction/p={0}/xip_CFHTAll.png' .format(p), dpi=1000, bbox_inches='tight')
    plt.clf()

    plt.figure(3).set_size_inches((8, 8), forward=False)
    plt.title("Correlation function varying $q$, $p=${0}" .format(p))
    plt.errorbar(thetasCFHT, ximCFHT, sigmCFHT, fmt='.k',  elinewidth=0.5, capsize=3)
    plt.legend()
    # os.system('mv figures/corrFunction/p={0}/xim.png figures/corrFunction/p={0}/xip_old.png' .format(p))
    plt.savefig('figures/corrFunction/p={0}/xim_CFHTAll.png' .format(p), dpi=1000, bbox_inches='tight')
    plt.clf()
    print("p={0}" .format(p))
# plt.show()
