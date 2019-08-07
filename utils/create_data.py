import os


def power3D_all(clean=False):
    if (not os.path.isfile("data/power_hm.dat")) or clean:
        os.system("mkdir -p data")
        os.system("./bin/halo_model_demo")


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


def findLmax(lmax, clean=False):

    if (not os.path.isfile("data/lmax={0}/xi1.dat" .format(lmax))) or clean:
        os.system('mkdir -p data/lmax={0}' .format(lmax))
        print("Creating data lmax={0}" .format(lmax))
        os.system('./bin/opt/findLmax {0}' .format(lmax))


def findLmax_CFHT(lmax, clean=False):

    if (not os.path.isfile("data/lmax={0}/xi1_CFHT.dat" .format(lmax))) or clean:
        os.system('mkdir -p data/lmax={0}' .format(lmax))
        print("Creating data lmax={0}" .format(lmax))
        os.system('./bin/opt/findLmax_CFHT {0}' .format(lmax))


def findL_length(l_length, clean=False):

    if (not os.path.isfile("data/l_length={0}/xi1.dat" .format(l_length))) or clean:
        os.system('mkdir -p data/l_length={0}' .format(l_length))
        print("Creating data l_length={0}" .format(l_length))
        os.system('./bin/opt/findL_length {0}' .format(l_length))


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


def xi_CFHT_mmin(mmins, l_max, clean=False):
    if mmins == []:
        mmins = ['1e7', '1e8', '1e9', '1e10', '1e11', '1e12', '1e13', '1e14', '1e15']

    for mmin in mmins:
        if (not os.path.isfile("data/mmin={0}/xi1.dat" .format(mmin))) or clean:
            os.system("mkdir -p data/mmin={0}" .format(mmin))
            print("Creating data for mmin={0}" .format(mmin))
            os.system("./bin/plots/xi_mmin {0} {1}" .format(*[mmin, l_max]))


def xi_CFHT_st(q, p, l_max, clean=False):
    if (not os.path.isfile('data/q={0}p={1}/xi1.dat' .format(*[q, p]))) or clean:
        print("Creating data for q={0} and p={1}" .format(*[q, p]))
        os.system('mkdir -p data/q={0}p={1}' .format(*[q, p]))
        os.system('./bin/plots/xi_st {0} {1} {2}' .format(*[q, p, l_max]))


def power_3D_ihm(ihms, clean=False):
    if ihms == []:
        ihms = [1, 3]

    for ihm in ihms:
        if (not os.path.isfile("data/ihm={0}/power_hm.dat" .format(ihm))) or clean:
            os.system("mkdir -p data/ihm={0}" .format(ihm))
            print("Creating data for ihm={0}" .format(ihm))
            os.system("./bin/plots/pow3D_ihm {0}" .format(ihm))


def power_2D_ihm(ihms, l_length, clean=False):
    if ihms == []:
        ihms = [1, 3]

    for ihm in ihms:
        if (not os.path.isfile("data/ihm={0}/power2D.dat" .format(ihm))) or clean:
            os.system("mkdir -p data/ihm={0}" .format(ihm))
            print("Creating data for ihm={0}" .format(ihm))
            os.system("./bin/plots/pow2D_ihm {0} {1}" .format(*[ihm, l_length]))


def xi_CFHT_ihm(ihms, l_max, icosmo=1, clean=False):
    if ihms == []:
        ihms = [1, 3]

    for ihm in ihms:
        if (not os.path.isfile("data/ihm={0}/xi1.dat" .format(ihm))) or clean:
            os.system("mkdir -p data/ihm={0}" .format(ihm))
            print("Creating data for ihm={0}" .format(ihm))
            os.system("./bin/plots/xi_ihm {0} {1} {2}" .format(*[ihm, l_max, icosmo]))


def xi_CFHT_sig8(sig8s, l_max, clean=False):
    if sig8s == []:
        sig8s = [0.7, 0.8]

    for sig8 in sig8s:
        if (not os.path.isfile("data/sig8={0}/xi1.dat" .format(sig8))) or clean:
            os.system("mkdir -p data/sig8={0}" .format(sig8))
            print("Creating data for sig8={0}" .format(sig8))
            os.system("./bin/plots/xi_sig8 {0} {1}" .format(*[sig8, l_max]))


def m_nu(clean=False):
    if (not os.path.isfile("data/quantities/nuM_10.dat")) or clean:
        os.system("mkdir -p data/quantities/")
        print("Creating data m(nu)")
        os.system("./bin/plots/m_nu")


def mass_function(q, p, scale, clean=False):
    if (not os.path.isfile("data/q={0}p={1}/a={2}/mass_function.dat" .format(*[q, p, scale]))) or clean:
        os.system("mkdir -p data/q={0}p={1}/a={2}" .format(*[q, p, scale]))
        print("Creating data mass function")
        os.system("./bin/plots/mass_function {0} {1} {2}" .format(*[q, p, scale]))


def multiplicity_function(q, p, scale, clean=False):
    if (not os.path.isfile("data/q={0}p={1}/a={2}/multiplicity_function.dat" .format(*[q, p, scale]))) or clean:
        os.system("mkdir -p data/q={0}p={1}/a={2}" .format(*[q, p, scale]))
        print("Creating data multiplicity function")
        os.system("./bin/plots/multiplicity_function {0} {1} {2}" .format(*[q, p, scale]))
