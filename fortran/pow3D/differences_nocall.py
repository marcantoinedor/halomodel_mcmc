import matplotlib.pyplot as plt
import os
from colour import Color

# import numpy as np

# x_axis
x_axis = []
data = open('data/mmaxe17/power_hm.dat', "r")
lines = data.readlines()
data.close()

for j in range(1, len(lines)-1):
    line = lines[j].split('       ')[1]
    x_axis.append(float(line.split('    ')[0]))
line = lines[len(lines)-1].split('      ')[1]
x_axis.append(float(line.split('    ')[0]))

# Scale parameter
n = 11
scale = []
for i in range(n-1):
    firstLine = lines[0].split('       ')
    scale.append(float(firstLine[i+3]))

# Index of scale parameter
a_i = 1

terms = ['1h', '2h', 'hm']
# mmaxs=['e17', 'e16', 'e15', 'e14', 'e13', 'e12', '5e11', 'e11']
mmaxs = ['e17', 'e16', '5e15', '2.5e15', 'e15', '5e14', 'e14', 'e13', 'e12', 'e11']

blue = Color("blue")
colors = list(blue.range_to(Color("green"), len(mmaxs)))
# print([color.split(' ')[1] for color in colors])

for parameter in scale:
    for term in terms:

        # collecting data
        index = 0
        datas = []

        for mmax in mmaxs:
            data = open('data/mmax{0}/power_{1}.dat' .format(*[mmax, term]), "r")
            lines = data.readlines()
            data.close()

            # parsing data

            column = []
            for j in range(1, len(lines)-1):
                line = lines[j].split('       ')[1]
                column.append(float(line.split('    ')[a_i]))
            line = lines[len(lines)-1].split('      ')[1]
            column.append(float(line.split('    ')[a_i]))

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
            if abs(datas[index][10] - datas[0][10])/datas[0][i] > 1/100:
                plt.plot(x_axis, [datas[index][i] for i in range(len(datas[0]))], color=colors[index].hex, label='$Mmax=${0}' .format(mmax))
                plt.title('{0} term power spectrum' .format(term))
                # plt.legend(loc=4, fontsize='x-small')
            if index == 0:
                plt.plot(x_axis, [datas[0][i] for i in range(len(datas[0]))], color="blue", linestyle='dashed', label='$Mmax=inf$')
                plt.legend(loc=4, fontsize='x-small')

            if mmax != 'e17':
                plt.subplot(212)
                # labels
                # logarithmic scale
                plt.xscale('log')
                plt.yscale('log')
                plt.xlabel('$k / h \ Mpc^{-1}$')
                plt.ylabel('$R(\Delta^2 (k))$')
                plt.plot(x_axis, [abs(datas[index][i] - datas[0][i])/datas[0][i] for i in range(len(datas[0]))], color=colors[index].hex, label='$Mmax=${0}' .format(mmax))
                plt.legend(bbox_to_anchor=(1.05, 1), loc=3, fontsize='x-small')
                # plt.title('Relative difference of {0} term' .format(term))

            # Incrementing index
            index += 1

        # plt.title('Relative difference of {0} term in power spectrum between infinite integral and finite integral' .format(term))

        # TkAgg backend
        manager = plt.get_current_fig_manager()
        manager.resize(*manager.window.maxsize())
        os.system("mkdir -p /home/marco/halo_model/figures/differences/a={0}" .format(parameter))
        plt.savefig('/home/marco/halo_model/figures/differences/a={0}/{1}.png' .format(*[parameter, term]), bbox_inches='tight')
        # plt.show()
        plt.clf()
    print('Scale parameter = {0}' .format(parameter))
    a_i += 1
