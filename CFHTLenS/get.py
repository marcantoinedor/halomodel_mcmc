import numpy as np


def thetas():
    data = open('CFHTLenS/readable.dat', "r")
    lines = data.readlines()
    data.close()
    thetas = []
    for j in range(1, len(lines)):
        line = lines[j].split('    ')[0]
        thetas.append(float(line))
    return np.array(thetas)


def xip():
    data = open('CFHTLenS/readable.dat', "r")
    lines = data.readlines()
    data.close()
    xips = []
    for j in range(1, len(lines)):
        line = lines[j].split('    ')[1]
        xips.append(float(line))
    return np.array(xips)


def xim():
    data = open('CFHTLenS/readable.dat', "r")
    lines = data.readlines()
    data.close()
    xims = []
    for j in range(1, len(lines)):
        line = lines[j].split('    ')[3]
        xims.append(float(line))
    return np.array(xims)


def cov_mat():
    data = open("CFHTLenS/cov.dat", "r")
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


def sigm():
    return ((cov_mat().diagonal())[:21])


def sigp():
    return ((cov_mat().diagonal())[21:])
