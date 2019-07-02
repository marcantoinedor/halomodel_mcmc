import os
import sys

if len(sys.argv) <= 3:
    exit(-1)
q = sys.argv[1]
p = sys.argv[2]
mmin = sys.argv[3]

if not os.path.isdir('data/mmin={0}' .format(mmin)):
    os.system('mkdir data/mmin={0}' .format(mmin))
    print("creating data mmin={0}" .format(mmin))
    os.system('./bin/halo_model_hm_mmin {0} {1} {2}' .format(*[q, p, mmin]))
