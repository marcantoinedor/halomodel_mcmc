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
q = 0.707
p = 0.3

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(a_s)))
i = 0
plt.figure(1).set_size_inches((16, 9), forward=False)
plt.xlabel('$M$')
plt.ylabel('$\\nu(M)$')
plt.yscale('log')
plt.xscale('log')
for index_a in range(1, len(a_s)+1):
    a = a_s[index_a-1]

    # plt.yticks(rotation=90)
    data = open('data/q={0}p={1}/nuM_{2}.dat' .format(*[q, p, index_a]))
    lines = data.readlines()
    columns = []

    x_axis = []
    for j in range(len(lines)-2):
        value = lines[j].split('   ')[1]
        x_axis.append(float(value.lower()))
    columns.append(x_axis)

    column = []
    for j in range(len(lines)-2):
        value = lines[j].split('       ')[1]
        column.append(float(value.lower()))
    columns.append(column)

    plt.plot(x_axis, column, color=colors[i].rgb, label="a={0}" .format(a))
    i += 1
    print("a=" + str(a))

plt.title("$\\nu(M)$")
plt.legend()
plt.savefig('figures/nu_m.png', bbox_inches='tight')
plt.clf()
# plt.show()
