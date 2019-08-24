import matplotlib.pyplot as plt
import os
from colour import Color
import numpy as np
import sys
import utils.create_data as create
import utils.get_quantities as dat


clean = False
if len(sys.argv) == 2:
    clean = (sys.argv[1] == 'clean')


SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

a_s = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]

qs = [0.6, 0.65, 0.707, 0.75, 0.8, 0.9]
ps = [0., 0.1, 0.2, 0.25, 0.3, 0.35, 0.4]

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(ps)))


for a in a_s:
    for q in qs:
        for p in ps:
            create.mass_function(q, p, a, clean=clean)

        plt.figure(1).set_size_inches((8, 8), forward=False)
        i = 0
        for p in ps:
            x_axis = dat.get_x_axis_mass_function(q, p, a)
            column = dat.get_column_mass_function(q, p, a)

            plt.plot(x_axis, x_axis*column, color=colors[i].rgb, label="p={0}" .format(p))
            i += 1

        plt.xscale('log')
        plt.xlabel('M')
        plt.ylabel('$Mn(M)$')
        plt.title("Mean mass function varying $p$, $q=${0}, $a=${1}" .format(*[q, a]))
        plt.legend()
        os.system('mkdir -p figures/quantities/q={0}/a={1}' .format(*[q, a]))
        plt.savefig('figures/quantities/q={0}/a={1}/mean_mass_function_p.png' .format(*[q, a]), dpi=200, bbox_inches='tight')
        plt.clf()
    print("a=" + str(a))
