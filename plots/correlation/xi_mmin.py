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

l_max = 100000
clean = False
q_st = 0.707
p_st = 0.3

# mmins = [7., 7.5, 8., 8.5, 9., 9.5, 10., 10.5, 11., 11.5, 12.5, 13., 13.5, 14., 14.5, 15.]
mmins = [7., 12, 12.5, 13., 13.5, 14., 14.5, 15.]

CFHT_data = True

thetasCFHT = get.get_theta_CFHT()
xipCFHT = get.get_xip_CFHT()
ximCFHT = get.get_xim_CFHT()
sigmCFHT = get.get_sigm_CFHT()
sigpCFHT = get.get_sigp_CFHT()

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(mmins)))

for mmin in mmins:
    if clean:
        os.system('mv -f data/q={0}p={1}/xi1_mmin={2}.dat data/q={0}p={1}/xi1_mmin={2}_old.dat'.format(*[q_st, p_st, mmin]))
        os.system('mv -f data/q={0}p={1}/xi3_mmin={2}.dat data/q={0}p={1}/xi3_mmin={2}_old.dat'.format(*[q_st, p_st, mmin]))

    if not os.path.isfile('data/q={0}p={1}/xi1_mmin={2}.dat'.format(*[q_st, p_st, mmin])):
        print("Creating data log_mmin={0}" .format(mmin))
        os.system('./bin/halo_model_plotxi_mmin {0} {1} 21 {2} {3}' .format(*[q_st, p_st, l_max, mmin]))

os.system('mkdir -p figures/mmin/')

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
data = open('data/q={0}p={1}/xi1_mmin={2}.dat' .format(*[q_st, p_st, mmins[0]]))
lines = data.readlines()
data.close()
for j in range(0, len(lines)):
    value = lines[j].split('  ')[1]
    x_axis.append(float(value.lower()))

for mmin in mmins:
    data = open('data/q={0}p={1}/xi1_mmin={2}.dat' .format(*[q_st, p_st, mmin]))
    lines1 = data.readlines()
    data.close()
    column1 = []

    data = open('data/q={0}p={1}/xi3_mmin={2}.dat' .format(*[q_st, p_st, mmin]))
    lines3 = data.readlines()
    data.close()
    column3 = []
    for j in range(0, len(lines)):
        value1 = lines1[j].split('        ')[1]
        column1.append(float(value1.lower()))
        value3 = lines3[j].split('        ')[1]
        column3.append(float(value3.lower()))

    plt.figure(1)
    plt.plot(x_axis, column1, color=colors[i].rgb, label="log(mmin)={0}" .format(mmin))
    plt.figure(3)
    plt.plot(x_axis, column3, color=colors[i].rgb, label="log(mmin)={0}" .format(mmin))
    i += 1

plt.figure(1).set_size_inches((8, 8), forward=False)
plt.title("Correlation function $\\xi_+$")
plt.legend()
if CFHT_data:
    plt.errorbar(thetasCFHT, xipCFHT, sigpCFHT, fmt='.k',  elinewidth=0.5, capsize=3)
    plt.savefig('figures/mmin/xip_CFHTmmin.png', dpi=1000, bbox_inches='tight')
else:
    plt.savefig('figures/mmin/xip_mmin.png', dpi=1000, bbox_inches='tight')

plt.figure(3).set_size_inches((8, 8), forward=False)
plt.title("Correlation function $\\xi_-$")
plt.legend()
if CFHT_data:
    plt.errorbar(thetasCFHT, ximCFHT, sigmCFHT, fmt='.k',  elinewidth=0.5, capsize=3)
    plt.savefig('figures/mmin/xim_CFHTmmin.png', dpi=1000, bbox_inches='tight')
else:
    plt.savefig('figures/mmin/xim_mmin.png', dpi=1000, bbox_inches='tight')
# plt.show()
