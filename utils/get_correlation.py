import os
import create_data as create
import CFHTLenS.get as dat


def get_x_axis_alpha():
    # it is simply CFHT data
    return dat.thetas()


def get_xip_alpha(alpha):
    '''
    float -> list
    returns the column of data
    '''

    data = open('data/alpha={0}/xi1.dat' .format(alpha))
    lines1 = data.readlines()
    data.close()
    column1 = []
    for line in lines1:
        column1.append(float(line.lower()))
    return column1


def get_xim_alpha(alpha):
    '''
    float -> list
    returns the column of data
    '''

    data = open('data/alpha={0}/xi3.dat' .format(alpha))
    lines3 = data.readlines()
    data.close()
    column3 = []
    for line in lines3:
        column3.append(float(line.lower()))
    return column3


def get_x_axis_mmin():
    # it is simply CFHT data
    return dat.thetas()


def get_xip_mmin(mmin):
    '''
    float -> list
    returns the column of data
    '''

    data = open('data/mmin={0}/xi1.dat' .format(mmin))
    lines1 = data.readlines()
    data.close()
    column1 = []
    for line in lines1:
        column1.append(float(line.lower()))
    return column1


def get_xim_mmin(mmin):
    '''
    float -> list
    returns the column of data
    '''

    data = open('data/mmin={0}/xi3.dat' .format(mmin))
    lines3 = data.readlines()
    data.close()
    column3 = []
    for line in lines3:
        column3.append(float(line.lower()))
    return column3


def get_x_axis_st():
    # it is simply CFHT data
    return dat.thetas()


def get_xip_st(q, p):
    '''
    float -> list
    returns the column of data
    '''

    data = open('data/q={0}p={1}/xi1.dat' .format(*[q, p]))
    lines1 = data.readlines()
    data.close()
    column1 = []
    for line in lines1:
        column1.append(float(line.lower()))
    return column1


def get_xim_st(q, p):
    '''
    float -> list
    returns the column of data
    '''

    data = open('data/q={0}p={1}/xi3.dat' .format(*[q, p]))
    lines3 = data.readlines()
    data.close()
    column3 = []
    for line in lines3:
        column3.append(float(line.lower()))
    return column3
