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

a_s = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]


for index_a in range(1, len(a_s)+1):
    a = a_s[index_a-1]
    q = 0.707
    ps = [0.0, 0.1, 0.2, 0.25, 0.3, 0.35, 0.4]

    for p in ps:
        os.system('python3 utils/create_data_st.py {0} {1}' .format(*[q, p]))

    begin_color = Color("blue")
    colors = list(begin_color.range_to(Color("green"), len(ps)))

    os.system('mkdir -p figures/multiplicity/a={0}' .format(a))

    plt.figure(1).set_size_inches((16, 9), forward=False)
    plt.xscale('log')
    plt.xlabel('M')
    plt.ylabel('$\\frac{M^2n(M)}{\\overline{\\rho}}$')
    # plt.yticks(rotation=90)
    i = 0
    for p in ps:
        data = open('data/q={0}p={1}/mult_{2}.dat' .format(*[q, p, index_a]))
        lines = data.readlines()
        columns = []

        x_axis = []
        for j in range(len(lines)-1):
            value = lines[j].split('   ')[1]
            x_axis.append(float(value.lower()))
        columns.append(x_axis)

        column = []
        for j in range(len(lines)-1):
            value = lines[j].split('        ')[1]
            column.append(float(value.lower()))
        columns.append(column)

        plt.plot(x_axis, column, color=colors[i].rgb, label="p={0}" .format(p))
        i += 1
    plt.title("Multiplicity function varying $p$, $q=${0}, $a=${1}" .format(*[q, a]))
    plt.legend()
    plt.savefig('figures/multiplicity/a={0}/multiplicity_p.png' .format(a), bbox_inches='tight')
    print("a=" + str(a))
# plt.show()
