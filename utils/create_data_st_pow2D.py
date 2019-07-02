import os
import sys

if len(sys.argv) <= 2:
    exit(-1)
q = sys.argv[1]
p = sys.argv[2]

if not os.path.isfile('data/q={0}p={1}/power2D.dat' .format(*[q, p])):
    os.system('mkdir -p data/q={0}p={1}' .format(*[q, p]))
    print("Creating data q={0}, p={1}" .format(*[q, p]))
    os.system('./bin/halo_model_2D {0} {1} 10000' .format(*[q, p]))
