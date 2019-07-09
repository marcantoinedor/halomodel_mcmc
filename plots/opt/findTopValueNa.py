import matplotlib.pyplot as plt
import numpy as np
from colour import Color
import os
import sys
import utils.create_data as create
import utils.get_opt as dat


clean = False
if len(sys.argv) == 2:
    clean = (sys.argv[1] == 'clean')

nas = [4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 18]
ref = 100

# l axis, the same as in the fortran code
lmin = 1
lmax = 1e4
nl = 100

ls = np.linspace(np.log(lmin), np.log(lmax), nl)
ls = np.exp(ls)

# Compute ref
create.findNa(ref, clean=clean)
refs = dat.get_Na(ref)

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(nas)))

i = 0
for na in nas:

    create.findNa(na, clean=clean)
    pow2D = dat.get_Na(na)

    # Plotting
    plt.figure(1)
    plt.plot(ls, [abs(pow2D[j] - refs[j])/refs[j] for j in range(len(refs))], color=colors[i].rgb, label="na={0}" .format(na))
    plt.legend()
    i += 1

plt.figure(1).set_size_inches((8, 8), forward=False)
plt.title('Variation of 2D-Power in Limber\'s integration with $n_aref$={0}' .format(ref))
plt.xlabel("$\ell$")
plt.ylabel("$(C_{\ell }-C_{\ell ref}) / C_{\ell ref}$")
plt.xscale('log')
plt.yscale('log')

plt.savefig('figures/opt/findNa.png', dpi=200, bbox_inches='tight')

plt.show()
