import matplotlib.pyplot as plt
import numpy as np
import mcmc.old_mcmcs.realMf as hm

compute = False
analyse = True
ps = np.arange(0., 0.49, 0.01)
qs = np.arange(0.3, 1.2, 0.01)

if compute:
    print("{0} computations needed" .format(len(ps)*len(qs)))

    nbr = len(ps)*len(qs)
    values = np.zeros((len(ps), len(qs)))

    i = 0
    for p in ps:
        j = 0
        for q in qs:
            if hm.CFHTv(21, q, p, parallel=True)[0] == -np.pi:
                print("Failure")
                values[i][j] = 1
            print("Iteration {0}/{1}" .format(*[(i+1)*(j+1), nbr]))
            j += 1
        i += 1

    np.save('code_issues/failure.npy', values)

if analyse:

    values = np.load('code_issues/failure.npy')
    matrix = values.reshape((len(ps), len(qs)))
    plt.imshow(matrix, extent=[0.3, 1.2, 0.49, 0.])
    plt.show()
