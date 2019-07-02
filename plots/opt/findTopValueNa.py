import matplotlib.pyplot as plt
import numpy as np
from colour import Color
import os

nas = [4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 18]
ref = 100
refs = []

pow2D = []

lmin = 1
lmax = 1e4
nl = 100

ls = np.linspace(np.log(lmin), np.log(lmax), nl)
ls = np.exp(ls)

os.system('mkdir -p data/na={0}' .format(ref))
os.system('./bin/halo_model_findNa {0}' .format(ref))

data = open("data/na={0}/pow2D.dat" .format(ref), "r")
lines = data.readlines()
data.close()
refs = [float(val) for val in lines]

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(nas)))

i = 0
for na in nas:
    os.system('mkdir -p data/na={0}' .format(na))
    os.system('./bin/halo_model_findNa {0}' .format(na))

    data = open("data/na={0}/pow2D.dat" .format(na), "r")
    lines = data.readlines()
    data.close()
    pow2D = [float(val) for val in lines]

    # Plotting
    plt.figure(1)
    plt.plot(ls, [abs(pow2D[j] - refs[j])/refs[j] for j in range(len(refs))], color=colors[i].rgb, label="na={0}" .format(na))
    plt.legend()
    i += 1

plt.figure(1).set_size_inches((8, 8), forward=False)
plt.title('Variation of 2D-Power in Limber integration with $n_aref$={0}' .format(ref))
plt.xlabel("$l$")
plt.ylabel("$(P_{2D}-P_{2Dref}) / P_{2Dref}$")
plt.xscale('log')
plt.yscale('log')
# plt.legend(bbox_to_anchor=(0.8, 1.25), loc='lower right')

plt.savefig('figures/findNa.png', bbox_inches='tight')

plt.show()
