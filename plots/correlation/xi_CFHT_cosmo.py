import matplotlib.pyplot as plt
import os
from colour import Color
import numpy as np
import CFHTLenS.get as dat

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 15

plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

icosmos = [1, 4, 42]
ihm = 3
cosmos = ['Fiducial', 'WMAP9', 'Planck 2018']

clean = False
q_st = 0.707
p_st = 0.3

x_axis = dat.thetas()
xipCFHT = dat.xip()
ximCFHT = dat.xim()
sigmCFHT = dat.sigm()
sigpCFHT = dat.sigp()


begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(icosmos)))

os.system('mkdir -p figures/icosmo/')

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

index = 0

for icosmo in icosmos:
    if clean:
        os.system('mv -f data/icosmo={0}/xi1_CFHT.dat data/icosmo{0}/xi1_CFHT_old.dat' .format(icosmo))
        os.system('mv -f data/icosmo={0}/xi3_CFHT.dat data/icosmo{0}/xi3_CFHT_old.dat' .format(icosmo))

    if not os.path.isfile('data/icosmo={0}/xi1_CFHT.dat'.format(icosmo)):
        print("Creating data icosmo={0}" .format(icosmo))
        os.system("mkdir -p data/icosmo={0}" .format(icosmo))
        os.system('./bin/cosmo_xi_CFHT {0} {1} {2} {3}' .format(*[q_st, p_st, icosmo, ihm]))

    data = open('data/icosmo={0}/xi1_CFHT.dat' .format(icosmo))
    lines1 = data.readlines()
    data.close()
    column1 = []

    data = open('data/icosmo={0}/xi3_CFHT.dat' .format(icosmo))
    lines3 = data.readlines()
    data.close()
    column3 = []
    for j in range(len(lines1)):
        column1.append(float(lines1[j].lower()))
        column3.append(float(lines3[j].lower()))

    plt.figure(1)
    plt.plot(x_axis, column1, color=colors[index].rgb, label="{0}" .format(cosmos[index]))
    plt.figure(3)
    plt.plot(x_axis, column3, color=colors[index].rgb, label="{0}" .format(cosmos[index]))
    index += 1

plt.figure(1).set_size_inches((8, 8), forward=False)
plt.title("Correlation function varying cosmology")
plt.errorbar(x_axis, xipCFHT, sigpCFHT, fmt='.k', elinewidth=0.5, capsize=3)
plt.legend()
# os.system('mv figures/corrFunction/q={0}/xip.png figures/corrFunction/q={0}/xip_old.png' .format(q))
plt.savefig('figures/icosmo/xip', dpi=200, bbox_inches='tight')

plt.figure(3).set_size_inches((8, 8), forward=False)
plt.title("Correlation function varying cosmology")
plt.errorbar(x_axis, ximCFHT, sigmCFHT, fmt='.k', elinewidth=0.5, capsize=3)
plt.legend()
# os.system('mv figures/corrFunction/q={0}/xim.png figures/corrFunction/q={0}/xip_old.png' .format(q))
plt.savefig('figures/icosmo/xim', dpi=200, bbox_inches='tight')
plt.show()
