import matplotlib.pyplot as plt
import os
import sys
from colour import Color
import utils.get_pow3D as dat
import utils.create_data as create

clean = False
if len(sys.argv) == 2:
    clean = (sys.argv[1] == 'clean')

# x_axis
x_axis = dat.get_x_axis_alpha()


# plots
terms = ['1h', '2h', 'hm', 'linear']

# Put the reference in the first position of this array for relative differences
alphas = [1.0, 0.6, 0.7, 0.8, 0.9]

# Create data
create.power3D_alpha(alphas, clean=clean)

# colors for the plot
begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(alphas)))

for term in terms:

    # collecting data
    index = 0
    datas = []

    for alpha in alphas:
        # Collect data
        column = dat.get_column_alpha(term, alpha)
        datas.append(column)

        # plotting
        plt.figure(1)

        plt.subplot(211)
        if max([abs(column[i] - datas[0][i])/datas[0][i] for i in range(len(column))]) > 1/100:
            plt.plot(x_axis, [column[i] for i in range(len(datas[0]))], color=colors[index].rgb, label='$\\alpha=${0}' .format(alpha))
            plt.title('{0} term power spectrum' .format(term))
        if index == 0:
            plt.plot(x_axis, [datas[0][i] for i in range(len(datas[0]))], color="blue", linestyle='dashed', label='$\\alpha=1.0$')

        if alpha != 1.0:
            plt.subplot(212)
            plt.plot(x_axis, [abs(column[i] - datas[0][i])/datas[0][i] for i in range(len(datas[0]))], color=colors[index].rgb, label='$\\alpha=${0}' .format(alpha))
        index += 1

    plt.subplot(211)
    plt.xscale('log')
    plt.yscale('log')
    plt.xlabel('$k / h \ Mpc^{-1}$')
    plt.ylabel('$\Delta^2 (k)$')
    plt.legend(bbox_to_anchor=(1.05, 1), loc=2, fontsize='x-small')

    plt.subplot(212)
    plt.xscale('log')
    plt.yscale('log')
    plt.xlabel('$k / h \ Mpc^{-1}$')
    plt.ylabel('$R(\Delta^2 (k))$')
    # plt.legend(bbox_to_anchor=(1.05, 1), loc=3, fontsize='x-small')

    os.system("mkdir -p figures/differences_alpha")
    plt.figure(1).set_size_inches((13, 9), forward=False)
    plt.savefig('figures/differences_alpha/{0}.png' .format(term), dpi=200, bbox_inches='tight')
    plt.clf()
    # plt.show()
