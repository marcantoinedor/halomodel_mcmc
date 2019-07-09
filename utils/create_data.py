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


def power3D_alpha(alphas, clean=False):

    if alphas == []:
        alphas = [0.6, 0.7, 0.8, 0.9, 1.0]

    for alpha in alphas:
        if (not os.path.isfile("data/alpha={0}/power_hm.dat" .format(alpha))) or clean:
            os.system("mkdir -p data/alpha={0}" .format(alpha))
            print("Creating data for alpha={0}" .format(alpha))
            os.system("./bin/plots/pow3D_alpha {0}" .format(alpha))


def power_2D_alpha(alphas, l_length, clean=False):
    if alphas == []:
        alphas = [0.6, 0.7, 0.8, 0.9, 1.0]
    for alpha in alphas:
        if (not os.path.isfile('data/alpha={0}/power2D.dat' .format(alpha))) or clean:
            os.system('mkdir -p data/alpha={0}' .format(alpha))
            print("Creating data alpha={0}" .format(alpha))
            os.system('./bin/plots/pow2D_alpha {0} {1}' .format(*[alpha, l_length]))


def xi_CFHT_alpha(alphas, l_max, clean=False):
    if alphas == []:
        alphas = [0.6, 0.7, 0.8, 0.9, 1.0]
    for alpha in alphas:
        if (not os.path.isfile('data/alpha={0}/xi1.dat' .format(alpha))) or clean:
            os.system('mkdir -p data/alpha={0}' .format(alpha))
            print("Creating data alpha={0}" .format(alpha))
            os.system('./bin/plots/xi_alpha {0} {1}' .format(*[alpha, l_max]))


def power_2D_mmin(mmins, l_length, clean=False):
    if mmins == []:
        mmins = ['1e7', '1e8', '1e9', '1e10', '1e11', '1e12', '1e13', '1e14', '1e15']

    for mmin in mmins:
        if (not os.path.isfile("data/mmin={0}/power2D.dat" .format(mmin))) or clean:
            os.system("mkdir -p data/mmin={0}" .format(mmin))
            print("Creating data for mmin={0}" .format(mmin))
            os.system("./bin/plots/pow2D_mmin {0} {1}" .format(*[mmin, l_length]))
