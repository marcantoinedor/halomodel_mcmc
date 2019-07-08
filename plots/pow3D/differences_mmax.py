import matplotlib.pyplot as plt
import os
from colour import Color
import utils.get as dat
import utils.create_data as create


x_axis = dat.get_x_axis_mmax()
scale = dat.get_scale_parameter()

# Index of scale parameter
a_i = 1

# terms to plot
terms = ['1h', '2h', 'hm']
# mmaxs=['e17', 'e16', 'e15', 'e14', 'e13', 'e12', '5e11', 'e11']
mmaxs = ['1e17', '5e16', '5e15', '2.5e15', '1e15', '5e14', '1e14', '1e13', '1e12']

# Create data if it doesn't exist
create.power3D_mmax(mmaxs)

# colors for the plot
begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(mmaxs)))

for parameter in scale:
    for term in terms:

        # collecting data
        index = 0
        datas = []

        for mmax in mmaxs:
            # Collect data
            datas.append(dat.get_pow3D_mmax(term, mmax, a_i))

            # plotting
            plt.figure(1)

            plt.subplot(211)
            # labels
            # logarithmic scale
            plt.xscale('log')
            plt.yscale('log')
            plt.xlabel('$k / h \ Mpc^{-1}$')
            plt.ylabel('$\Delta^2 (k)$')
            if abs(datas[index][10] - datas[0][10])/datas[0][10] > 1/100:
                plt.plot(x_axis, [datas[index][i] for i in range(len(datas[0]))], color=colors[index].rgb, label='$Mmax=${0}' .format(mmax))
                plt.title('{0} term power spectrum' .format(term))
                # plt.legend(loc=4, fontsize='x-small')
            if index == 0:
                plt.plot(x_axis, [datas[0][i] for i in range(len(datas[0]))], color="blue", linestyle='dashed', label='$Mmax=\\infty$')
                plt.legend(loc=4, fontsize='x-small')

            if mmax != '1e17':
                plt.subplot(212)
                # labels
                # logarithmic scale
                plt.xscale('log')
                plt.yscale('log')
                plt.xlabel('$k / h \ Mpc^{-1}$')
                plt.ylabel('$R(\Delta^2 (k))$')
                plt.plot(x_axis, [abs(datas[index][i] - datas[0][i])/datas[0][i] for i in range(len(datas[0]))], color=colors[index].rgb, label='$Mmax=${0}' .format(mmax))
                plt.legend(bbox_to_anchor=(1.05, 1), loc=3, fontsize='x-small')
                # plt.title('Relative difference of {0} term' .format(term))

            # Incrementing index
            index += 1

        # plt.title('Relative difference of {0} term in power spectrum between infinite integral and finite integral' .format(term))

        os.system("mkdir -p figures/differences_mmax/a={0}" .format(parameter))
        plt.figure(1).set_size_inches((13, 9), forward=False)
        plt.savefig('figures/differences_mmax/a={0}/{1}.png' .format(*[parameter, term]), dpi=300, bbox_inches='tight')
        # plt.show()
        plt.clf()
    print('Scale parameter = {0}' .format(parameter))
    a_i += 1
