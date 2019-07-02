import matplotlib.pyplot as plt
import numpy as np
from colour import Color
import os

lmaxs = [1e3, 1e4, 5e4, 1e5, 5e5]

ref = 1e6
refs1 = []
refs3 = []

xi1 = []
xi3 = []

thmin = 0.5*1/60*np.pi/180
thmax = 100*1/60*np.pi/180
nth = 100

ths = np.linspace(np.log(thmin), np.log(thmax), nth)
ths = np.exp(ths)

thsarcmin = 180/np.pi*60*ths
thsdegree = thsarcmin/60

os.system('mkdir -p data/lmax={0}' .format(ref))
os.system('./bin/halo_model_findL 10 {0}' .format(ref))

data = open("data/lmax={0}/xi1.dat" .format(ref), "r")
lines = data.readlines()
data.close()
refs1 = [float(val) for val in lines]

data = open("data/lmax={0}/xi3.dat" .format(ref), "r")
lines = data.readlines()
data.close()
refs3 = [float(val) for val in lines]

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(lmaxs)))

i = 0
for l in lmaxs:
    os.system('mkdir -p data/lmax={0}' .format(l))
    os.system('./bin/halo_model_findL 10 {0}' .format(l))

    data = open("data/lmax={0}/xi1.dat" .format(l), "r")
    lines = data.readlines()
    data.close()
    xi1 = [float(val) for val in lines]

    # Plotting
    plt.figure(1)
    plt.plot(thsarcmin, [abs(xi1[j] - refs1[j])/refs1[j] for j in range(len(refs1))], color=colors[i].rgb, label="$l$={0}" .format(l))
    plt.legend()

    data = open("data/lmax={0}/xi3.dat" .format(l), "r")
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

plt.savefig('figures/findLtop_plus1.png', bbox_inches='tight')

plt.figure(3).set_size_inches((8, 8), forward=False)
plt.title("Variation of $\\xi_-$ changing modes number, lref={0}" .format(ref))
plt.xlabel("$\\theta$ (arcmin)")
plt.ylabel("$(\\xi_--\\xi_m)/ \\xi_m$")
plt.yscale('log')
plt.xscale('log')

# plt.legend(bbox_to_anchor=(0.8, 1.25), loc='lower right')

plt.savefig('figures/findLtop_minus1.png', bbox_inches='tight')

plt.show()
