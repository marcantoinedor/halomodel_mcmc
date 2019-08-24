import matplotlib.pyplot as plt
import os
import sys
from colour import Color
import numpy as np
import utils.get_pow3D as dat
import utils.create_data as create

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

clean = False
if len(sys.argv) == 2:
    clean = (sys.argv[1] == 'clean')

terms = ['hm', '1h', '2h', 'linear']
a_s = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
ihms = [1, 3]
halomodels = ["HMcode", "Halo-Model"]

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(ihms)))

x_axis = dat.get_x_axis_ihm(clean=clean)

create.power_3D_ihm(ihms, clean=clean)

for term in terms:
    index_a = 1
    for a in a_s:
        plt.figure(1).set_size_inches((8, 8), forward=False)
        os.system('mkdir -p figures/power/a={0}/ihm/' .format(a))
        i = 0
        for ihm in ihms:
            column = dat.get_column_ihm(term, ihm, index_a)
            plt.plot(x_axis, column, color=colors[i].rgb, label="{0}" .format(halomodels[i]))
            i += 1

        plt.title("{1} term in power spectrum, $a=${0}" .format(*[a, term]))
        plt.xlabel('$k / h \ Mpc^{-1}$')
        plt.ylabel('$\Delta^2 (k)$')
        plt.xscale('log')
        plt.yscale('log')
        plt.legend()

        plt.savefig('figures/power/a={0}/ihm/power_{1}.png' .format(*[a, term]), dpi=200, bbox_inches='tight')
        plt.clf()
        print("This was for a={0}\n" .format(a))
        index_a += 1
    print("This was for {0} term\n\n" .format(term))
