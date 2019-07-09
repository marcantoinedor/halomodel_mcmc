import os
import create_data as create


def get_Na(na):

    data = open("data/na={0}/pow2D.dat" .format(na), "r")
    lines = data.readlines()
    data.close()
    refs = [float(val) for val in lines]
    return refs
