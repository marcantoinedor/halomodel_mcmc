import os
import create_data as create


def get_x_axis_mmax():
    mmax = '1e17'
    term = 'hm'
    create.power3D_mmax([mmax])
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


def get_scale_parameter():
    mmax = '1e17'
    term = 'hm'
    os.system("python3 utils/create_data_mmax.py {0}" .format(mmax))
    data = open("data/mmax{0}/power_{1}.dat" .format(*[mmax, term]), "r")

    lines = data.readlines()
    data.close()

    n = 11
    scale = []
    for i in range(n-1):
        firstLine = lines[0].split('       ')
        scale.append(float(firstLine[i+3]))
    return scale


def get_pow3D_mmax(term, mmax, iscale):
    '''
    str * str * int -> list
    giving term in ['hm', '1h', '2h', 'linear'], mmax and scale parameter index, returns the column of data
    '''

    data = open("data/mmax{0}/power_{1}.dat" .format(*[mmax, term]), "r")
    lines = data.readlines()
    data.close()

    # parsing data
    column = []
    for j in range(1, len(lines)-1):
        line = lines[j].split('       ')[1]
        column.append(float(line.split('    ')[iscale]))
    line = lines[len(lines)-1].split('      ')[1]
    column.append(float(line.split('    ')[iscale]))
    return column


def get_x_axis_st(term, q, p):

    create.power_3D_st(q, p)

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


def get_column_mmax(term, mmax, scale):
    '''
    str * str * float -> list
    giving term in ['hm', '1h', '2h', 'linear'], mmax and scale parameter, returns the column of data
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
