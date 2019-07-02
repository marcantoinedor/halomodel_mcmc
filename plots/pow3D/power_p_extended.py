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

terms = ['hm', '1h', '2h']
a_s = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
qs = [0.6, 0.65, 0.707, 0.75, 0.8, 0.9]
ps = [0., 0.1, 0.2, 0.25, 0.3, 0.35, 0.4]
term = '2h'

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(ps)))

p_st = 0.3
q_st = 0.707
x_axis = get.get_x_axis_st(term, q_st, p_st, a_s[0])

index_a = 0
for a in a_s:
    for q in qs:
        for p in ps:
            os.system('python3 utils/create_data_st.py {0} {1}' .format(*[q, p]))

        os.system('mkdir -p figures/power/a={1}/q={0}' .format(*[q, a]))

        plt.figure(1).set_size_inches((16, 9), forward=False)
        plt.xscale('log')
        plt.xlabel('$k / h \ Mpc^{-1}$')
        plt.ylabel('$\Delta^2 (k)$')
        plt.yscale('log')
        # plt.yticks(rotation=90)
        i = 0
        for p in ps:
            column = get.get_column_st(term, q, p, a)
            plt.plot(x_axis, column, color=colors[i].rgb, label="p={0}" .format(p))
            i += 1
        plt.title("Power spectrum varying $p$, $q=${0}, $a=${1}" .format(*[q, a]))
        plt.legend()
        plt.savefig('figures/power/a={1}/q={0}/power_{2}_p.png' .format(*[q, a, term]), bbox_inches='tight')
        plt.clf()
    print("a=" + str(a))
# plt.show()
