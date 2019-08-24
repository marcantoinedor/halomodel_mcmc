import matplotlib.pyplot as plt
import os
import sys
from colour import Color
import numpy as np
import utils.get_quantities as dat
import create_data as create

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

clean = False
if len(sys.argv) == 2:
    clean = (sys.argv[1] == 'clean')

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), 10))
i = 0

# creating data
create.m_nu(clean=clean)

plt.figure(1).set_size_inches((8, 8), forward=False)
for index_a in range(1, 11):
    a = index_a/10
    x_axis = dat.get_Nu_M_nu()
    column = dat.get_M_M_nu(index_a)
    plt.plot(x_axis, column, color=colors[i].rgb, label="a={0}" .format(a))
    i += 1
    print("a=" + str(a))

plt.xlabel('$\\nu$')
plt.ylabel('$M(\\nu)$')
plt.yscale('log')
plt.legend()
os.system("mkdir -p figures/quantities")
plt.savefig('figures/quantities/m_nu.png', bbox_inches='tight')
