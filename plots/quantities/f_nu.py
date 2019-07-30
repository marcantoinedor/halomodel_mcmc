import matplotlib.pyplot as plt
import numpy as np
from scipy.special import gamma
import scipy as cp
import os
from colour import Color


def normalisation(q, p):
    '''
            float float -> float
    '''
    return ((cp.sqrt(np.pi/(2*q))+cp.sqrt(q)*2**(-p)*gamma(0.5-p))**(-1))


def prob(nu, q, p):
    '''
            float, float, float -> float
    '''
    infactor = 1+(q*nu*nu)**(-p)
    return infactor*cp.exp(-q*nu*nu/2)


p_st = 0.3
q_st = 0.707
A_st = normalisation(q_st, p_st)

p_range = np.array([-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.45])
q_range = np.array([0.5, 0.6, 0.707, 0.8, 0.9, 1.0, 1.1])

x_axis = np.linspace(0.01, 5, 1000)


# colors for the plot
begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(p_range)))


for q in q_range:

    index = 0
    os.system("mkdir -p figures/quantities/q={0}" .format(q))

    plt.figure(1)
    plt.xscale('log')
    plt.yscale('log')
    plt.xlabel("$\\nu$")
    plt.ylabel("$f_{ST}(\\nu)$")

    plt.figure(2)
    plt.yscale('log')
    plt.xlabel("$\\nu$")
    plt.ylabel("$f_{ST}(\\nu)$")

    plt.figure(3)
    plt.xlabel("$\\nu$")
    plt.ylabel("$f_{ST}(\\nu)$")

    for p in p_range:
        plt.figure(1)
        norm = normalisation(q, p)
        plots = [prob(x, q, p)/norm for x in x_axis]
        if p == 0.3:
            plt.plot(x_axis, plots, color=colors[index].rgb, linestyle='dashed', label="p={0}" .format(p))
        else:
            plt.plot(x_axis, plots, color=colors[index].rgb, label="p={0}" .format(p))

        plt.figure(2)
        if p == 0.3:
            plt.plot(x_axis, plots, color=colors[index].rgb, linestyle='dashed', label="p={0}" .format(p))
        else:
            plt.plot(x_axis, plots, color=colors[index].rgb, label="p={0}" .format(p))

        plt.figure(3)
        if p == 0.3:
            plt.plot(x_axis, plots, color=colors[index].rgb, linestyle='dashed', label="p={0}" .format(p))
        else:
            plt.plot(x_axis, plots, color=colors[index].rgb, label="p={0}" .format(p))
        print('p='+str(p))
        index += 1

    plt.figure(1).set_size_inches((8, 8), forward=False)
    plt.title("Mass function, q={0} fixed" .format(q))
    plt.legend()
    plt.savefig('figures/quantities/q={0}/f_nu_loglog.png' .format(q), dpi=200)
    plt.clf()
    plt.figure(2).set_size_inches((8, 8), forward=False)
    plt.title("Mass function, q={0} fixed" .format(q))
    plt.legend()
    plt.savefig('figures/quantities/q={0}/f_nu_log.png' .format(q), dpi=200)
    plt.clf()
    plt.figure(3).set_size_inches((8, 8), forward=False)
    plt.title("Mass function, q={0} fixed" .format(q))
    plt.legend()
    plt.savefig('figures/quantities/q={0}/f_nu_classic.png' .format(q), dpi=200)
    plt.clf()

    print(q)


# exchange roles
# colors for the plot
begin_color = Color("red")
colors = list(begin_color.range_to(Color("blue"), len(q_range)))

for p in p_range:

    index = 0
    os.system("mkdir -p figures/quantities/p={0}" .format(p))

    plt.figure(1)
    plt.xscale('log')
    plt.yscale('log')
    plt.xlabel("$\\nu$")
    plt.ylabel("$f_{ST}(\\nu)$")

    plt.figure(2)
    plt.yscale('log')
    plt.xlabel("$\\nu$")
    plt.ylabel("$f_{ST}(\\nu)$")

    plt.figure(3)
    plt.xlabel("$\\nu$")
    plt.ylabel("$f_{ST}(\\nu)$")

    for q in q_range:
        plt.figure(1)
        norm = normalisation(q, p)
        plots = [prob(x, q, p)/norm for x in x_axis]
        if q == 0.707:
            plt.plot(x_axis, plots, color=colors[index].rgb, linestyle='dashed', label="q={0}" .format(q))
        else:
            plt.plot(x_axis, plots, color=colors[index].rgb, label="q={0}" .format(q))
        plt.legend()

        plt.figure(2)
        if q == 0.707:
            plt.plot(x_axis, plots, color=colors[index].rgb, linestyle='dashed', label="q={0}" .format(q))
        else:
            plt.plot(x_axis, plots, color=colors[index].rgb, label="q={0}" .format(q))
        plt.legend()

        plt.figure(3)
        if q == 0.707:
            plt.plot(x_axis, plots, color=colors[index].rgb, linestyle='dashed', label="q={0}" .format(q))
        else:
            plt.plot(x_axis, plots, color=colors[index].rgb, label="q={0}" .format(q))
        plt.legend()

        print('q='+str(q))
        index += 1

    plt.figure(1).set_size_inches((8, 8), forward=False)
    plt.title("Mass function, p={0} fixed" .format(p))
    plt.legend()
    plt.savefig('figures/quantities/p={0}/f_nu_loglog.png' .format(p), dpi=200)
    plt.clf()
    plt.figure(2).set_size_inches((8, 8), forward=False)
    plt.title("Mass function, p={0} fixed" .format(p))
    plt.legend()
    plt.savefig('figures/quantities/p={0}/f_nu_log.png' .format(p), dpi=200)
    plt.clf()
    plt.figure(3).set_size_inches((8, 8), forward=False)
    plt.title("Mass function, p={0} fixed" .format(p))
    plt.legend()
    plt.savefig('figures/quantities/p={0}/f_nu_classic.png' .format(p), dpi=200)
    plt.clf()
    print(p)
