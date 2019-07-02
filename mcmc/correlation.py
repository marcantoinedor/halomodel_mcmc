import numpy as np
import subprocess as sub


def xiCFHT(q, p, icosmo, ihm, verbose=False):
    if verbose:
        print("q={0}, p={1}" .format(*[q, p]))
    hm = "./bin/xi_CFHT"
    args = [str(q), str(p), str(icosmo), str(ihm)]
    # Calling fortran code as a subprocess
    proc = sub.Popen([hm, args[0], args[1], args[2], args[3]], stdout=sub.PIPE)
    # Launching code
    out, err = proc.communicate()

    if err:
        print("q={0}, p={1} : {2}" .format(*[q, p, err]))
        return np.array([-np.pi])
    result = np.array(out.decode('ascii').split(), dtype=float)
    return result
