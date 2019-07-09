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
