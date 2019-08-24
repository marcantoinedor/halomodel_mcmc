import matplotlib.pyplot as plt
import os
import sys
from colour import Color
import utils.get_pow3D as dat
import utils.create_data as create
import numpy as np

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

# Creating data
create.power3D_all()

# Index of scale parameter
iscale = 1
scale = dat.get_scale_parameter()

# x_axis
x_axis = dat.get_x_axis_all()

# plots
terms = ['1h', '2h', 'hm', 'linear']

plt.figure(1).set_size_inches((8, 8), forward=False)
for parameter in scale:

    for term in terms:
        data = dat.get_column_all(term, iscale)
        # from Delta(k) to P(k)
        data = [(2*np.pi)**3/(x_axis[j]**3*4*np.pi)*data[j] for j in range(len(x_axis))]
        plt.plot(x_axis, data, label="{0}" .format(term))
    plt.xscale('log')
    plt.yscale('log')
    plt.xlabel('$k / h \ Mpc^{-1}$')
    plt.ylabel('$P(k)$')
    plt.legend()
    os.system("mkdir -p figures/power/a={0}" .format(parameter))
    plt.savefig("figures/power/a={0}/all_P.png" .format(parameter), dpi=200, bbox_inches='tight')
    plt.clf()
    print('Scale parameter = {0}' .format(parameter))
    iscale += 1
