import os
import create_data as create


def get_x_axis_st(q, p):
    create.power_2D_st(q, p)
    x_axis = []
    data = open('data/q={0}p={1}/power2D.dat' .format(*[q, p]), "r")
    lines = data.readlines()
    data.close()

    for j in range(1, len(lines)):
        value = lines[j].split('  ')[1]
        x_axis.append(float(value.lower()))
    return x_axis


def get_data_st(q, p):
    '''
    float * float-> list
    giving term in ['hm', '1h', '2h'], q_st and p_st and scale parameter, returns the column of data
    '''

    data = open("data/q={0}p={1}/power2D.dat" .format(*[q, p]), "r")
    lines = data.readlines()
    data.close()

    column = []
    for j in range(1, len(lines)):
        value = lines[j].split('        ')[1]
        column.append(float(value.lower()))
    return column


def get_x_axis_alpha(l_length):
    create.power_2D_alpha([1.0], l_length)
    x_axis = []
    data = open('data/alpha=1.0/power2D.dat')
    lines = data.readlines()
    data.close()
    for j in range(0, len(lines)):
        value = lines[j].split('  ')[1]
        x_axis.append(float(value.lower()))
    return x_axis


def get_data_alpha(alpha):
    '''
    float -> list
    returns the column of data
    '''

    data = open("data/alpha={0}/power2D.dat" .format(alpha), "r")
    lines1 = data.readlines()
    data.close()
    column1 = []
    for j in range(len(lines1)):
        value1 = lines1[j].split('        ')[1]
        column1.append(float(value1.lower()))
    return column1


def get_x_axis_mmin(l_length):
    create.power_2D_mmin([1e7], l_length)
    x_axis = []
    data = open('data/mmin=1e7/power2D.dat')
    lines = data.readlines()
    data.close()
    for j in range(len(lines)):
        value = lines[j].split('  ')[1]
        x_axis.append(float(value.lower()))
    return x_axis


def get_data_mmin(mmin):
    '''
    float -> list
    returns the column of data
    '''

    data = open("data/mmin={0}/power2D.dat" .format(mmin), "r")
    lines1 = data.readlines()
    data.close()
    column1 = []
    for j in range(len(lines1)):
        value1 = lines1[j].split('        ')[1]
        column1.append(float(value1.lower()))
    return column1


def get_data_ihm(ihm):
    '''
    float -> list
    returns the column of data
    '''

    data = open("data/ihm={0}/power2D.dat" .format(ihm), "r")
    lines1 = data.readlines()
    data.close()
    column1 = []
    for j in range(len(lines1)):
        value1 = lines1[j].split('        ')[1]
        column1.append(float(value1.lower()))
    return column1


def get_x_axis_ihm(l_length):
    create.power_2D_ihm([1], l_length)
    x_axis = []
    data = open('data/ihm=1/power2D.dat')
    lines = data.readlines()
    data.close()
    for j in range(len(lines)):
        value = lines[j].split('  ')[1]
        x_axis.append(float(value.lower()))
    return x_axis
