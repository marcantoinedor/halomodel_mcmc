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

a = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
q = 0.9
p = 0.2

# os.system('mkdir -p data/q={0}p={1}' .format(*[q, p]))
os.system('python3 utils/create_data_st.py {0} {1}' .format(*[q, p]))

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(a)))

os.system('mkdir -p figures/multiplicity')

plt.figure(1).set_size_inches((16, 9), forward=False)
plt.xscale('log')
plt.xlabel('log(M)')
plt.ylabel('$\\frac{M^2n(M)}{\\overline{\\rho}}$')
# plt.yticks(rotation=90)

for i in range(1, len(a)+1):
    data = open('data/q={0}p={1}/mult_{2}.dat' .format(*[q, p, i]))
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

    plt.plot(x_axis, column, color=colors[i-1].rgb, label="a={0}" .format(a[i-1]))

plt.title("Multiplicity function for different $a$")
plt.legend()
plt.savefig('figures/multiplicity/multiplicityST_redshift.png', bbox_inches='tight')

plt.show()
