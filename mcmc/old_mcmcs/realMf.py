import numpy as np
import scipy as cp
import os
from math import *
# def mass_function(m, q, p, a):
#     string = "./bin/halo_model {0} {1} {2} {3}" .format(*[q, p, m, a])
#     number = (subprocess.check_output(string, shell=True))[1:].lower()
#     # number = str(float(number))
#     print(number)
#     return float(number)


def mass_function(m, q, p, a):
    string = "./bin/halo_model {0} {1} {2} {3}" .format(
        *[q, p, m, a])
    os.system(string)
    data = open('data/data.dat', 'r')
    value = data.readlines()
    data.close()
    value = float(value[0])
    print(value)
    return(value)


# def mass_functionv(mass, q, p, a):
#     return [mass_function(m, q, p, a) for m in mass]

def mass_functionv(mass_length, q, p, a):
    # name = np.random.randint(1, 10000)
    name = 1
    string = "./bin/halo_model.mf {0} {1} {2} {3} {4}" .format(
        *[q, p, mass_length, a, name])
    os.system(string)
    data = open('data/data_{0}.dat' .format(name), 'r')
    value = data.readlines()
    data.close()
    return [float(value[i]) for i in range(mass_length)]


def multiplicity_functionv(mass_length, q, p, a):
    name = 2
    string = "./bin/halo_model.mult {0} {1} {2} {3} {4}" .format(
        *[q, p, mass_length, a, name])
    os.system(string)
    data = open('data/data_{0}.dat' .format(name), 'r')
    value = data.readlines()
    data.close()
    return [float(value[i]) for i in range(mass_length)]


def mean_functionv(mass_length, q, p, a):
    name = 3
    string = "./bin/halo_model.mean {0} {1} {2} {3} {4}" .format(
        *[q, p, mass_length, a, name])
    os.system(string)
    data = open('data/data_{0}.dat' .format(name), 'r')
    value = data.readlines()
    data.close()
    return [float(value[i]) for i in range(mass_length)]


def power_spectrumv(k_length, q, p, a):
    name = 4
    string = "./bin/halo_model.k {0} {1} {2} {3} {4}" .format(
        *[q, p, k_length, a, name])
    os.system(string)
    data = open('data/data_{0}.dat' .format(name), 'r')
    value = data.readlines()
    data.close()
    return [float(value[i]) for i in range(k_length)]


def power_spectrum2Dv(l_length, q, p, parallel=False):
    if parallel:
        binary = np.random.randint(50)
        name = np.random.randint(6, 1000000000)
        string = "./bin/halo_model.2D.{4} {0} {1} {2} {3}" .format(
            *[q, p, l_length, name, binary])
    else:
        name = 5
        string = "./bin/halo_model.2D {0} {1} {2} {3}" .format(
            *[q, p, l_length, name])

    os.system(string)
    # to override errors in halo model calculation
    i = 0
    while not os.path.isfile('data/data_{0}.dat' .format(name)) and i <= 100000:
        i += 1
    if i > 100000:
        return [-np.inf for i in range(l_length)]
    data = open('data/data_{0}.dat' .format(name), 'r')
    value = data.readlines()
    data.close()
    os.system('rm -f data/data_{0}.dat' .format(name))

    return [float(value[i]) for i in range(l_length)]


def xiv(th_length, q, p, parallel=False):
    if parallel:
        binary = np.random.randint(5000)
        name = np.random.randint(6, 1000000000)
        string = "./bin/halo_model.CFHT1.{4} {0} {1} {2} {3}" .format(
            *[q, p, th_length, name, binary])
    else:
        name = 6
        string = "./bin/halo_model.CFHT1 {0} {1} {2} {3}" .format(
            *[q, p, th_length, name])

    os.system(string)
    # to override errors in halo model calculation
    i = 0
    while (not os.path.isfile('data/data1_{0}.dat' .format(name))) and i <= 100000:
        i += 1
    if i > 100000:
        return np.array([[-np.inf, -np.inf] for i in range(th_length)])
    data1 = open('data/data1_{0}.dat' .format(name), 'r')
    value1 = data1.readlines()
    data1.close()
    # os.system('rm -f data/data1_{0}.dat' .format(name))

    data3 = open('data/data3_{0}.dat' .format(name), 'r')
    value3 = data3.readlines()
    data3.close()
    # os.system('rm -f data/data3_{0}.dat' .format(name))
    # out = np.array([float(value1[i] for i in range(th_length))], [
    #                float(value3[i] for i in range(th_length))])
    return np.array([[float(value1[i]), float(value3[i])] for i in range(th_length)])


def CFHTv(th_length, q, p, parallel=False):
    print("q={0}, p={1}" .format(*[q, p]))
    if parallel:
        binary = np.random.randint(100)
        name = np.random.randint(6, 1000000000)
        string = "./bin/halo_model.CFHT1.{4} {0} {1} {2} {3}" .format(
            *[q, p, th_length, name, binary])
    else:
        name = 6
        string = "./bin/halo_model.CFHT1 {0} {1} {2} {3}" .format(
            *[q, p, th_length, name])

    # creating data
    os.system(string)

    # to override errors in halo model calculation
    if not os.path.isfile('data/data_{0}.dat' .format(name)):
        a = np.array([-np.pi])
        return a
    # overriding disk errors
    i = 0
    while (sum(1 for line in open('data/data_{0}.dat' .format(name))) < 2*th_length) and i <= 1000000:
        i += 1
    data = open('data/data_{0}.dat' .format(name), 'r')
    value = data.readlines()
    data.close()
    # os.system('rm -f data/data_{0}.dat' .format(name))
    if len(value) < 2*th_length:
        return np.array([np.pi])
    return np.array([float(value[i]) for i in range(2*th_length)])


def CFHTvPlot(th_length, q, p, parallel=False):
    print("q={0}, p={1}" .format(*[q, p]))

    if os.path.isfile('data/p={0},q={1}' .format(*[p, q])):
        data = open('data/p={0},q={1}' .format(*[p, q]), 'r')
        value = data.readlines()
        data.close()
        return np.array([float(value[i]) for i in range(2*th_length)])

    # creating datapoints
    name = np.random.randint(1000)
    string = "./bin/halo_model.CFHT1 {0} {1} {2} {3}" .format(
        *[q, p, th_length, name])

    os.system(string)
    # to override errors in halo model calculation
    i = 0
    if not os.path.isfile('data/data_{0}.dat' .format(name)):
        return np.array([-np.pi])
    if (sum(1 for line in open('data/data_{0}.dat' .format(name))) < 2*th_length):
        return np.array([np.pi])

    data = open('data/data_{0}.dat' .format(name), 'r')
    value = data.readlines()
    data.close()
    os.system('mv data/data_{0}.dat data/p={1},q={2}' .format(*[name, p, q]))

    return np.array([float(value[i]) for i in range(2*th_length)])


def CFHTmminv(th_length, q, p, mmin, parallel=False):
    print("q={0}, p={1}, mmin={2}" .format(*[q, p, mmin]))
    if parallel:
        binary = np.random.randint(100)
        name = np.random.randint(6, 1000000000)
        string = "./bin/halo_model.mminCFHT.{5} {0} {1} {4} {2} {3}" .format(
            *[q, p, th_length, name, mmin, binary])
    else:
        name = 7
        string = "./bin/halo_model.mminCFHT {0} {1} {4} {2} {3}" .format(
            *[q, p, th_length, name, mmin])

    # creating data
    os.system(string)

    # to override errors in halo model calculation
    if not os.path.isfile('data/data_{0}.dat' .format(name)):
        a = np.array([-np.pi])
        return a
    # overriding disk errors
    i = 0
    while (sum(1 for line in open('data/data_{0}.dat' .format(name))) < 2*th_length) and i <= 1000000:
        i += 1
    data = open('data/data_{0}.dat' .format(name), 'r')
    value = data.readlines()
    data.close()
    # os.system('rm -f data/data_{0}.dat' .format(name))
    if len(value) < 2*th_length:
        return np.array([np.pi])
    return np.array([float(value[i]) for i in range(2*th_length)])


def get_theta_CFHT():
    data = open('../halo_model/utils/CFHT.dat', "r")
    lines = data.readlines()
    data.close()
    thetas = []
    for j in range(1, len(lines)):
        line = lines[j].split('    ')[0]
        thetas.append(float(line))
    return thetas


def get_xip_CFHT():
    data = open('../halo_model/utils/CFHT.dat', "r")
    lines = data.readlines()
    data.close()
    xips = []
    for j in range(1, len(lines)):
        line = lines[j].split('    ')[1]
        xips.append(float(line))
    return xips


def get_xim_CFHT():
    data = open('../halo_model/utils/CFHT.dat', "r")
    lines = data.readlines()
    data.close()
    xims = []
    for j in range(1, len(lines)):
        line = lines[j].split('    ')[3]
        xims.append(float(line))
    return xims


def get_cov_mat_CFHT():
    data = open("CFHT/cov.dat", "r")
    lines = data.readlines()
    data.close()
    cov = np.zeros((len(lines), len(lines)), dtype=float)
    i = 0
    j = 0
    for line in lines:
        elements = line.split(' ')
        for j in range(len(lines)):
            cov[i][j] = float(elements[j])
        i += 1
    return cov


def get_sigm_CFHT():
    return ((get_cov_mat_CFHT().diagonal())[:21])


def get_sigp_CFHT():
    return ((get_cov_mat_CFHT().diagonal())[21:])
