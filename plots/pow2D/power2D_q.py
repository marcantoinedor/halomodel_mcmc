import matplotlib.pyplot as plt
import os
from colour import Color
import numpy as np

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

clean = False

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(qs)))

if clean:
    os.system('rm data/q=*/power2D.dat')

for p in ps:
    for q in qs:
        os.system('python3 utils/create_data_st_pow2D.py {0} {1}' .format(*[q, p]))

    os.system('mkdir -p figures/power2D/p={0}' .format(p))

    plt.figure(1).set_size_inches((8, 8), forward=False)
    plt.xscale('log')
    plt.xlabel('$\ell$')
    plt.ylabel('$P_{2D}$')
    plt.yscale('log')

    # plt.yticks(rotation=90)
    i = 0
    for q in qs:
        data = open('data/q={0}p={1}/power2D.dat' .format(*[q, p]))
        lines = data.readlines()
        columns = []

        x_axis = []
        for j in range(1, len(lines)):
            value = lines[j].split('  ')[1]
            x_axis.append(float(value.lower()))
        columns.append(x_axis)

        column = []
        for j in range(1, len(lines)):
            value = lines[j].split('        ')[1]
            column.append(float(value.lower()))
        columns.append(column)

        plt.plot(x_axis, column, color=colors[i].rgb, label="q={0}" .format(q))
        i += 1
    plt.title("2D power spectrum varying $q$, $p=${0}" .format(p))
    plt.legend()
    plt.savefig('figures/power2D/p={0}/power_q.png' .format(p), bbox_inches='tight')
    plt.clf()
    print(p)
# plt.show()
