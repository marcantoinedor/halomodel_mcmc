import os


def power3D_mmax(mmaxs, clean=False):

    if mmaxs == []:
        mmaxs = ['1e17', '5e16', '5e15', '2.5e15', '1e15', '5e14', '1e14', '1e13', '1e12', '1e11']

    for mmax in mmaxs:
        if (not os.path.isfile("data/mmax{0}/power_hm.dat" .format(mmax))) or clean:
            os.system("mkdir -p data/mmax{0}" .format(mmax))
            print("Creating data for mmax={0}" .format(mmax))
            if mmax[0] == 'e':
                os.system("./bin/plots/pow3D_mmax 1{0}" .format(mmax))
            else:
                os.system("./bin/plots/pow3D_mmax {0}" .format(mmax))


def power_3D_st(q, p, clean=False):

    if (not os.path.isfile('data/q={0}p={1}/power_hm.dat' .format(*[q, p]))) or clean:
        print("Creating data for q={0} and p={1}" .format(*[q, p]))
        os.system('mkdir -p data/q={0}p={1}' .format(*[q, p]))
        os.system('./bin/plots/pow3D_st {0} {1}' .format(*[q, p]))


def power_2D_st(q, p, clean=False):

    if (not os.path.isfile('data/q={0}p={1}/power2D.dat' .format(*[q, p]))) or clean:
        os.system('mkdir -p data/q={0}p={1}' .format(*[q, p]))
        print("Creating data q={0}, p={1}" .format(*[q, p]))
        os.system('./bin/plots/pow2D_st {0} {1} 10000' .format(*[q, p]))
