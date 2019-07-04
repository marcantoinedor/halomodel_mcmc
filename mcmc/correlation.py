import numpy as np
import subprocess as sub


def xiCFHT(q, p, icosmo, ihm, verbose=False):
    if verbose:
        print("q={0}, p={1}" .format(*[q, p]))
    hm = "./bin/xi_CFHT"
    args = [str(q), str(p), str(icosmo), str(ihm)]
    # Calling fortran code as a subprocess
    proc = sub.Popen([hm, args[0], args[1], args[2], args[3]], stdout=sub.PIPE, stderr=sub.PIPE)
    # Launching code
    out, err = proc.communicate()

    if err:
        print("q={0}, p={1} : \n\n{2}" .format(*[q, p, err]))
        return np.array([-np.pi])
    result = np.array(out.decode('ascii').split(), dtype=float)
    return result


def xi_ampCFHT(alpha, icosmo, ihm, verbose=False):
    if verbose:
        print("alpha={0}" .format(alpha))
    hm = "./bin/xi_amp_CFHT"
    args = [str(alpha), str(icosmo), str(ihm)]
    # Calling fortran code as a subprocess
    proc = sub.Popen([hm, args[0], args[1], args[2]], stdout=sub.PIPE)
    # Launching code
    out, err = proc.communicate()

    if err:
        print("alpha={0}: {1}" .format(*[alpha, err]))
        return np.array([-np.pi])
    result = np.array(out.decode('ascii').split(), dtype=float)


def xi_mminCFHT(mmin_log, icosmo, ihm, verbose=False):
    if verbose:
        print("mmin_log={0}" .format(alpha))
    hm = "./bin/xi_mmin_CFHT"
    args = [str(mmin), str(icosmo), str(ihm)]
    # Calling fortran code as a subprocess
    proc = sub.Popen([hm, args[0], args[1], args[2]], stdout=sub.PIPE)
    # Launching code
    out, err = proc.communicate()

    if err:
        print("mmin_log={0}: {1}" .format(*[mmin_log, err]))
        return np.array([-np.pi])
    result = np.array(out.decode('ascii').split(), dtype=float)
