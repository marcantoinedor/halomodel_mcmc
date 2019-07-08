import matplotlib.pyplot as plt
import os
import sys
from colour import Color
import numpy as np
import utils.get as dat
import utils.create_data as create

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 15

plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

clean = False
if len(sys.argv) == 2:
    clean = (sys.argv[1] == 'True')

terms = ['hm', '1h', '2h']
a_s = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
qs = [0.6, 0.65, 0.707, 0.75, 0.8, 0.9]
ps = [0., 0.1, 0.2, 0.25, 0.3, 0.35, 0.4]

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(qs)))

p_st = 0.3
q_st = 0.707

x_axis = dat.get_x_axis_st('hm', q_st, p_st)

for term in terms:
    index_a = 0
    for a in a_s:
        for p in ps:
            os.system('mkdir -p figures/power/a={1}/p={0}' .format(*[p, a]))
            plt.figure(1).set_size_inches((8, 8), forward=False)
            i = 0
            for q in qs:
                create.power_3D_st(q, p, clean=clean)
                column = dat.get_column_st(term, q, p, a)
                plt.plot(x_axis, column, color=colors[i].rgb, label="q={0}" .format(q))
                i += 1

            plt.title("Power spectrum varying $q$, $p=${0}, $a=${1}" .format(*[p, a]))
            plt.xlabel('$k / h \ Mpc^{-1}$')
            plt.ylabel('$\Delta^2 (k)$')
            plt.xscale('log')
            plt.yscale('log')
            plt.legend()

            plt.savefig('figures/power/a={1}/p={0}/power_{2}_p.png' .format(*[p, a, term]), dpi=200, bbox_inches='tight')
            plt.clf()

            print("q={0}, p={1}" .format(*[q, p]))
        clean = False
        print("This was for a={0}\n" .format(a))
    print("This was for {0} term\n\n" .format(term))
