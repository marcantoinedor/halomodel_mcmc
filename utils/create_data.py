import os
import sys

if len(sys.argv) <= 1:
    mmaxs = ['1e17', '5e16', '5e15', '2.5e15', '1e15', '5e14', '1e14', '1e13', '1e12', '1e11']
else:
    mmaxs = [sys.argv[i] for i in range(1, len(sys.argv))]

for mmax in mmaxs:
    if not os.path.isdir("data/mmax{0}" .format(mmax)):
        os.system("mkdir data/mmax{0}" .format(mmax))
        if mmax[0] == 'e':
            os.system("./bin/halo_model 1{0}" .format(mmax))
        else:
            os.system("./bin/halo_model {0}" .format(mmax))
        os.system("mv data/*.dat data/mmax{0}" .format(mmax))
