import matplotlib.pyplot as plt
import os
from colour import Color

# x_axis
x_axis = []
data = open('data/mmin=1e7/k.dat', "r")
lines = data.readlines()
data.close()

for j in range(len(lines)):
    x_axis.append(float(lines[j]))

q_st = 0.707
p_st = 0.3

# plots
terms = ['1h', '2h', 'hm']
# mmins=['e17', 'e16', 'e15', 'e14', 'e13', 'e12', '5e11', 'e11']
mmins = ['1e7', '1e8', '1e9', '1e10', '1e11', '1e12', '1e13', '1e14', '1e15']

clean = True
# Create data if it doesn't exist
for mmin in mmins:
    if clean:
        os.system('rm -rf data/mmin={0}' .format(mmin))
    os.system("python3 utils/create_data_mmin.py {0} {1} {2}" .format(*[q_st, p_st, mmin]))

# colors for the plot
begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(mmins)))

for term in terms:

    # collecting data
    index = 0
    datas = []

    for mmin in mmins:
        data = open("data/mmin={0}/pow_{1}.dat" .format(*[mmin, term]), "r")
        lines = data.readlines()
        data.close()

        # parsing data

        column = []
        for j in range(len(lines)):
            column.append(float(lines[j]))

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
            # plt.legend(loc=4, fontsize='x-small')

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
        # plt.title('Relative difference of {0} term' .format(term))
        index += 1
    # plt.title('Relative difference of {0} term in power spectrum between infinite integral and finite integral' .format(term))

    os.system("mkdir -p figures/differences_mmin")
    plt.figure(1).set_size_inches((13, 9), forward=False)
    plt.savefig('figures/differences_mmin/{0}.png' .format(term), dpi=1000, bbox_inches='tight')
    plt.clf()
    # plt.show()
