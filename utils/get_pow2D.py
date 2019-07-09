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
