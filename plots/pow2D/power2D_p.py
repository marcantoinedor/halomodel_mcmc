import matplotlib.pyplot as plt
import os
import sys
from colour import Color
import utils.get_pow2D as get
import utils.create_data as create

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 15

plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

# qs = [0.707]
# ps = [0.3]

qs = [0.6, 0.65, 0.707, 0.75, 0.8, 0.9]
ps = [0., 0.1, 0.2, 0.25, 0.3, 0.35, 0.4]

clean = False
if len(sys.argv) == 2:
    clean = (sys.argv[1] == 'clean')


begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(ps)))

x_axis = get.get_x_axis_st(qs[0], ps[0])
for q in qs:
    i = 0
    for p in ps:
        create.power_2D_st(q, p, clean=clean)
        column = get.get_data_st(q, p)

        plt.plot(x_axis, column, color=colors[i].rgb, label="p={0}" .format(p))
        i += 1

    os.system('mkdir -p figures/power2D/q={0}' .format(q))

    plt.figure(1).set_size_inches((8, 8), forward=False)
    plt.title("2D power spectrum varying $p$, $q=${0}" .format(q))
    plt.legend()
    plt.xscale('log')
    plt.yscale('log')
    plt.xlabel('$\ell$')
    plt.ylabel('$C_\ell$')
    plt.savefig('figures/power2D/q={0}/power_p.png' .format(q), dpi=200, bbox_inches='tight')
    plt.clf()
    print(q)
# plt.show()
