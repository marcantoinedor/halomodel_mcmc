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

q_st = 0.707
p_st = 0.3
begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(a_s)))

plt.figure(1).set_size_inches((8, 8), forward=False)
i = 0
for a in a_s:

    create.mass_function(q_st, p_st, a, clean=clean)

    x_axis = dat.get_x_axis_mass_function(q_st, p_st, a)
    column = dat.get_column_mass_function(q_st, p_st, a)

    plt.plot(x_axis, x_axis*column, color=colors[i].rgb, label="a={0}" .format(a))
    i += 1

plt.xscale('log')
plt.yscale('log')
plt.xlabel('M')
plt.ylabel('$Mn(M)$')
plt.title("Mean mass function")
plt.legend()
os.system('mkdir -p figures/quantities/')
plt.savefig('figures/quantities/mean_mass_function.png', dpi=200, bbox_inches='tight')
