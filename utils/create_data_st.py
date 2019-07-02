import os
import sys

if len(sys.argv) <= 2:
    exit(-1)
q = sys.argv[1]
p = sys.argv[2]

if not os.path.isdir('data/q={0}p={1}' .format(*[q, p])):
    os.system('mkdir data/q={0}p={1}' .format(*[q, p]))
    os.system('./bin/halo_model_printall {0} {1}' .format(*[q, p]))
