import matplotlib.pyplot as plt
import numpy as np
from colour import Color
import os
import get

lmaxs = [10000, 50000, 100000, 500000, 1000000]

ref = 5000000
refs1 = []
refs3 = []

xi1 = []
xi3 = []

clean = False
thsarcmin = np.array(get.get_theta_CFHT())
thsdeg = thsarcmin/60

os.system('mkdir -p data/lmax={0}' .format(ref))
if not os.path.isfile("data/lmax={0}/xi1_CFHT.dat" .format(ref)):

    print("Creating reference")
    os.system('./bin/halo_model_findL_CFHT 21 {0}' .format(ref))

data = open("data/lmax={0}/xi1_CFHT.dat" .format(ref), "r")
lines = data.readlines()
data.close()
refs1 = [float(val) for val in lines]

data = open("data/lmax={0}/xi3_CFHT.dat" .format(ref), "r")
lines = data.readlines()
data.close()
refs3 = [float(val) for val in lines]

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(lmaxs)))

i = 0
for l in lmaxs:
    os.system('mkdir -p data/lmax={0}' .format(l))
    if clean:
        os.system("mv data/lmax={0}/xi1_CFHT.dat data/lmax={0}/xi1_CFHT_old.dat" .format(l))
        os.system("mv data/lmax={0}/xi3_CFHT.dat data/lmax={0}/xi3_CFHT_old.dat" .format(l))

    if not os.path.isfile("data/lmax={0}/xi1_CFHT.dat" .format(l)):
        print("Creating {0}" .format(l))
        os.system('./bin/halo_model_findL_CFHT 21 {0}' .format(l))

    data = open("data/lmax={0}/xi1_CFHT.dat" .format(l), "r")
    lines = data.readlines()
    data.close()
    xi1 = [float(val) for val in lines]

    # Plotting
    plt.figure(1)
    plt.plot(thsarcmin, [abs(xi1[j] - refs1[j])/refs1[j] for j in range(len(refs1))], color=colors[i].rgb, label="$l$={0}" .format(l))
    plt.legend()

    data = open("data/lmax={0}/xi3_CFHT.dat" .format(l), "r")
    lines = data.readlines()
    data.close()
    xi3 = [float(val) for val in lines]

    # Plotting
    plt.figure(3)
    plt.plot(thsarcmin, [abs(xi3[j] - refs3[j])/refs3[j] for j in range(len(refs1))], color=colors[i].rgb, label="$l$={0}" .format(l))
    plt.legend()
    i += 1

plt.figure(1).set_size_inches((8, 8), forward=False)
plt.title("Variation of $\\xi_+$ changing modes number, lref={0}" .format(ref))
plt.xlabel("$\\theta$ (arcmin)")
plt.ylabel("$(\\xi_+-\\xi_m)/ \\xi_m$")
plt.yscale('log')
plt.xscale('log')

# plt.legend(bbox_to_anchor=(0.8, 1.25), loc='lower right')

plt.savefig('figures/findLtop_plus_CFHT.png', bbox_inches='tight')

plt.figure(3).set_size_inches((8, 8), forward=False)
plt.title("Variation of $\\xi_-$ changing modes number, lref={0}" .format(ref))
plt.xlabel("$\\theta$ (arcmin)")
plt.ylabel("$(\\xi_--\\xi_m)/ \\xi_m$")
plt.yscale('log')
plt.xscale('log')

# plt.legend(bbox_to_anchor=(0.8, 1.25), loc='lower right')

plt.savefig('figures/findLtop_minus_CFHT.png', bbox_inches='tight')

plt.show()
