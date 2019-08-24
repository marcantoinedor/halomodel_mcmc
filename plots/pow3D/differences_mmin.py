import matplotlib.pyplot as plt
import os
import sys
from colour import Color
import utils.get_pow3D as dat
import utils.create_data as create

SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels

clean = False
if len(sys.argv) == 2:
    clean = (sys.argv[1] == 'clean')

# x_axis
x_axis = dat.get_x_axis_mmin()


# plots
terms = ['1h', '2h', 'hm', 'linear']
mmins = ['1e7', '1e8', '1e9', '1e10', '1e11', '1e12', '1e13', '1e14', '1e15']

# Create data
create.power3D_mmin(mmins, clean=clean)

# colors for the plot
begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(mmins)))

for term in terms:

    # collecting data
    index = 0
    datas = []

    for mmin in mmins:
        column = dat.get_column_mmin(term, mmin)
        datas.append(column)
        # plotting
        plt.figure(1)

        plt.subplot(211)
        # labels
        # logarithmic scale
        plt.xscale('log')
        plt.yscale('log')
        plt.xlabel('$k / h \ Mpc^{-1}$')
        plt.ylabel('$\Delta^2 (k)$')
        if max([abs(column[i] - datas[0][i])/datas[0][i] for i in range(len(column))]) > 1/100:
            plt.plot(x_axis, [column[i] for i in range(len(datas[0]))], color=colors[index].rgb, label='$Mmin=${0}' .format(mmin))
            plt.title('{0} term power spectrum' .format(term))
            # plt.legend(loc=4, fontsize='x-small')
        if index == 0:
            plt.plot(x_axis, [datas[0][i] for i in range(len(datas[0]))], color="blue", linestyle='dashed', label='$Mmin=1e7$')
            plt.legend(bbox_to_anchor=(1.05, 1), loc=3, fontsize='x-small')

        if mmin != '1e7':
            plt.subplot(212)
            # labels
            # logarithmic scale
            plt.xscale('log')
            plt.yscale('log')
            plt.xlabel('$k / h \ Mpc^{-1}$')
            plt.ylabel('$R(\Delta^2 (k))$')
            plt.plot(x_axis, [abs(column[i] - datas[0][i])/datas[0][i] for i in range(len(datas[0]))], color=colors[index].rgb, label='$Mmin=${0}' .format(mmin))
            plt.legend(bbox_to_anchor=(1.05, 1), loc=3, fontsize='x-small')
        index += 1

    os.system("mkdir -p figures/differences_mmin")
    plt.figure(1).set_size_inches((13, 9), forward=False)
    plt.savefig('figures/differences_mmin/{0}.png' .format(term), dpi=200, bbox_inches='tight')
    plt.clf()
    # plt.show()
