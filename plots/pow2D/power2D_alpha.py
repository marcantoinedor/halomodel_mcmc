import matplotlib.pyplot as plt
import os
import sys
from colour import Color
import utils.create_data as create
import utils.get_pow2D as dat

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 15

plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

# length of l_array
l_length = 10000

clean = False
if len(sys.argv) == 2:
    clean = (sys.argv[1] == 'clean')

alphas = [0.6, 0.7, 0.8, 0.9, 1.0]

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(alphas)))

# Create data
create.power_2D_alpha(alphas, l_length, clean=clean)

# Get x_axis for plot
x_axis = dat.get_x_axis_alpha(l_length)

plt.figure(1).set_size_inches((8, 8), forward=False)

i = 0
for alpha in alphas:
    column = dat.get_data_alpha(alpha)
    plt.plot(x_axis, column, color=colors[i].rgb, label="alpha={0}" .format(alpha))
    i += 1

os.system('mkdir -p figures/power2D/')

plt.xlabel('$\ell$')
plt.ylabel('$C(\ell)$')
plt.xscale('log')
plt.yscale('log')
plt.title("2D-power spectrum")
plt.legend()
plt.savefig('figures/power2D/pow2D_alpha.png', dpi=200, bbox_inches='tight')
plt.show()
