import os
import create_data as create


def get_Na(na):

    data = open("data/na={0}/pow2D.dat" .format(na), "r")
    lines = data.readlines()
    data.close()
    refs = [float(val) for val in lines]
    return refs


def get_xip(lmax):

    data = open("data/lmax={0}/xi1.dat" .format(lmax), "r")
    lines = data.readlines()
    data.close()
    val1 = [float(val) for val in lines]
    return val1


def get_xim(lmax):

    data = open("data/lmax={0}/xi3.dat" .format(lmax), "r")
    lines = data.readlines()
    data.close()
    val3 = [float(val) for val in lines]
    return val3


def get_xip_CFHT(lmax):

    data = open("data/lmax={0}/xi1_CFHT.dat" .format(lmax), "r")
    lines = data.readlines()
    data.close()
    val1 = [float(val) for val in lines]
    return val1


def get_xim_CFHT(lmax):

    data = open("data/lmax={0}/xi3_CFHT.dat" .format(lmax), "r")
    lines = data.readlines()
    data.close()
    val3 = [float(val) for val in lines]
    return val3


def get_xip_l_length(l_length):
    data = open("data/l_length={0}/xi1.dat" .format(l_length), "r")
    lines = data.readlines()
    data.close()
    vals1 = [float(val) for val in lines]
    return vals1


def get_xim_l_length(l_length):
    data = open("data/l_length={0}/xi3.dat" .format(l_length), "r")
    lines = data.readlines()
    data.close()
    vals1 = [float(val) for val in lines]
    return vals1
