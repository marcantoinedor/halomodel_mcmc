import os


def power3D_mmin(mmins, clean=False):

    if mmins == []:
        mmins = ['1e17', '5e16', '5e15', '2.5e15', '1e15', '5e14', '1e14', '1e13', '1e12', '1e11']

    for mmin in mmins:
        if (not os.path.isfile("data/mmin{0}/power_hm.dat" .format(mmin))) or clean:
            os.system("mkdir -p data/mmin{0}" .format(mmin))
            print("Creating data for mmin={0}" .format(mmin))
            if mmin[0] == 'e':
                os.system("./bin/plots/pow3D_mmin 1{0}" .format(mmin))
            else:
                os.system("./bin/plots/pow3D_mmin {0}" .format(mmin))


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


def findNa(na, clean=False):

    if (not os.path.isfile("data/na={0}/pow2D.dat" .format(na))) or clean:
        os.system('mkdir -p data/na={0}' .format(na))
        print("Creating data na={0}" .format(na))
        os.system('./bin/opt/findNa {0}' .format(na))


def power3D_mmin(mmins, clean=False):

    if mmins == []:
        mmins = ['1e7', '1e8', '1e9', '1e10', '1e11', '1e12', '1e13', '1e14', '1e15']

    for mmin in mmins:
        if (not os.path.isfile("data/mmin={0}/power_hm.dat" .format(mmin))) or clean:
            os.system("mkdir -p data/mmin={0}" .format(mmin))
            print("Creating data for mmin={0}" .format(mmin))
            os.system("./bin/plots/pow3D_mmin {0}" .format(mmin))
