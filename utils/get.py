import os
from math import *


def get_column(term, mmax, scale):
    os.system("python3 utils/create_data.py {0}" .format(mmax))
    data = open("data/mmax{0}/power_{1}.dat" .format(*[mmax, term]), "r")

    lines = data.readlines()
    data.close()
    x_axis = []
    for j in range(1, len(lines)-1):
        line = lines[j].split('       ')[1]
        x_axis.append(float(line.split('    ')[0]))
    line = lines[len(lines)-1].split('      ')[1]
    x_axis.append(float(line.split('    ')[0]))
    return x_axis


def get_x_axis_st(term, q, p, scale):

    os.system("python3 utils/create_data_st.py {0} {1}" .format(*[q, p]))
    data = open("data/q={0}p={1}/power_{2}.dat" .format(*[q, p, term]), "r")

    lines = data.readlines()
    data.close()
    x_axis = []
    for j in range(1, len(lines)-1):
        line = lines[j].split('       ')[1]
        x_axis.append(float(line.split('    ')[0]))
    line = lines[len(lines)-1].split('      ')[1]
    x_axis.append(float(line.split('    ')[0]))
    return x_axis


def get_column(term, mmax, scale):
    '''
    str * str * float -> list
    giving term in ['hm', '1h', '2h'], mmax and scale parameter, returns the column of data
    '''

    os.system("python3 utils/create_data.py {0}" .format(mmax))

    data = open("data/mmax{0}/power_{1}.dat" .format(*[mmax, term]), "r")
    lines = data.readlines()
    data.close()

    column = []
    index_scale = int(10*float(scale)) - 1
    # parsing data

    for j in range(1, len(lines)-1):
        line = lines[j].split('       ')[1]
        column.append(float(line.split('    ')[index_scale]))
    line = lines[len(lines)-1].split('      ')[1]
    column.append(float(line.split('    ')[index_scale]))

    return column


def get_column_st(term, q, p, scale):
    '''
    str * float * float * float -> list
    giving term in ['hm', '1h', '2h'], q_st and p_st and scale parameter, returns the column of data
    '''

    os.system("python3 utils/create_data_st.py {0} {1}" .format(*[q, p]))

    data = open("data/q={0}p={1}/power_{2}.dat" .format(*[q, p, term]), "r")
    lines = data.readlines()
    data.close()

    column = []
    index_scale = int(10*float(scale)) - 1
    # parsing data

    for j in range(1, len(lines)-1):
        line = lines[j].split('       ')[1]
        column.append(float(line.split('    ')[index_scale]))
    line = lines[len(lines)-1].split('      ')[1]
    column.append(float(line.split('    ')[index_scale]))

    return column


def get_theta_CFHT():
    data = open('utils/CFHT.dat', "r")
    lines = data.readlines()
    data.close()
    thetas = []
    for j in range(1, len(lines)):
        line = lines[j].split('    ')[0]
        thetas.append(float(line))
    return thetas


def get_xip_CFHT():
    data = open('utils/CFHT.dat', "r")
    lines = data.readlines()
    data.close()
    xips = []
    for j in range(1, len(lines)):
        line = lines[j].split('    ')[1]
        xips.append(float(line))
    return xips


def get_xim_CFHT():
    data = open('utils/CFHT.dat', "r")
    lines = data.readlines()
    data.close()
    xims = []
    for j in range(1, len(lines)):
        line = lines[j].split('    ')[3]
        xims.append(float(line))
    return xims


def get_sigm_CFHT():
    data = open('utils/CFHT.dat', "r")
    lines = data.readlines()
    data.close()
    sigms = []
    for j in range(1, len(lines)):
        line = lines[j].split('    ')[2]
        sigms.append(sqrt(float(line)))
    return sigms


def get_sigp_CFHT():
    data = open('utils/CFHT.dat', "r")
    lines = data.readlines()
    data.close()
    sigps = []
    for j in range(1, len(lines)):
        line = lines[j].split('    ')[4]
        sigps.append(sqrt(float(line)))
    return sigps
