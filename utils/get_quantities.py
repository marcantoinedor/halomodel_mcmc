import numpy as np


def get_Nu_M_nu(iscale=1):
    data = open('data/quantities/Mnu_{0}.dat' .format(iscale))
    lines = data.readlines()
    data.close()
    nu = []
    for j in range(len(lines)-1):
        value = lines[j].split('  ')[1]
        nu.append(float(value.lower()))

    return np.array(nu)


def get_M_M_nu(iscale):
    data = open('data/quantities/Mnu_{0}.dat' .format(iscale))
    lines = data.readlines()
    data.close()
    m = []
    for j in range(len(lines)-1):
        value = lines[j].split('        ')[1]
        m.append(float(value.lower()))

    return np.array(m)


def get_M_nu_M(iscale=1):
    data = open('data/quantities/nuM_{0}.dat' .format(iscale))
    lines = data.readlines()
    data.close()
    m = []
    for j in range(len(lines)-2):
        value = lines[j].split('   ')[1]
        m.append(float(value.lower()))

    return np.array(m)


def get_Nu_nu_M(iscale):
    data = open('data/quantities/nuM_{0}.dat' .format(iscale))
    lines = data.readlines()
    data.close()
    nu = []
    for j in range(len(lines)-2):
        value = lines[j].split('       ')[1]
        nu.append(float(value.lower()))

    return np.array(nu)


def get_x_axis_mass_function(q, p, scale):
    data = open('data/q={0}p={1}/a={2}/mass.dat' .format(*[q, p, scale]))
    lines = data.readlines()
    data.close()
    x_axis = []
    for line in lines:
        x_axis.append(float(line))
    return np.array(x_axis)


def get_column_mass_function(q, p, scale):
    data = open('data/q={0}p={1}/a={2}/mass_function.dat' .format(*[q, p, scale]))
    lines = data.readlines()
    data.close()
    x_axis = []
    for line in lines:
        x_axis.append(float(line))
    return np.array(x_axis)


def get_x_axis_multiplicity_function(q, p, scale):
    data = open('data/q={0}p={1}/a={2}/mass.dat' .format(*[q, p, scale]))
    lines = data.readlines()
    data.close()
    x_axis = []
    for line in lines:
        x_axis.append(float(line))
    return np.array(x_axis)


def get_column_multiplicity_function(q, p, scale):
    data = open('data/q={0}p={1}/a={2}/multiplicity_function.dat' .format(*[q, p, scale]))
    lines = data.readlines()
    data.close()
    column = []
    for line in lines:
        column.append(float(line))
    return np.array(column)
