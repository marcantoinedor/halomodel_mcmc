import matplotlib.pyplot as plt
import numpy as np
from colour import Color
import os
import sys
import utils.create_data as create
import utils.get_opt as dat
import CFHTLenS.get as get

clean = False
if len(sys.argv) == 2:
    clean = (sys.argv[1] == 'clean')

lmaxs = [int(1e3), int(1e4), int(5e4), int(1e5), int(5e5)]
ref = int(1e6)

thsarcmin = get.thetas()

# create reference data
create.findLmax_CFHT(ref, clean=clean)

refsp = dat.get_xip_CFHT(ref)
refsm = dat.get_xim_CFHT(ref)

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(lmaxs)))

i = 0
for l in lmaxs:
    # create data and collect it
    create.findLmax_CFHT(l, clean=clean)
    xip = dat.get_xip_CFHT(l)
    xim = dat.get_xim_CFHT(l)

    # Plotting
    plt.figure(1)
    plt.plot(thsarcmin, [abs(xip[j] - refsp[j])/refsp[j] for j in range(len(refsp))], color=colors[i].rgb, label="$l$={0}" .format(l))
    plt.legend()

    # Plotting
    plt.figure(3)
    plt.plot(thsarcmin, [abs(xim[j] - refsm[j])/refsm[j] for j in range(len(refsp))], color=colors[i].rgb, label="$l$={0}" .format(l))
    plt.legend()
    i += 1

plt.figure(1).set_size_inches((8, 8), forward=False)
plt.title("Variation of $\\xi_+$ changing modes number, lref={0}" .format(ref))
plt.xlabel("$\\theta$ (arcmin)")
plt.ylabel("$(\\xi_+-\\xi_{ref})/ \\xi_{ref}$")
plt.yscale('log')
plt.xscale('log')
plt.savefig('figures/opt/findmaxP_CFHT.png', bbox_inches='tight')

plt.figure(3).set_size_inches((8, 8), forward=False)
plt.title("Variation of $\\xi_-$ changing modes number, lref={0}" .format(ref))
plt.xlabel("$\\theta$ (arcmin)")
plt.ylabel("$(\\xi_--\\xi_{ref})/ \\xi_{ref}$")
plt.yscale('log')
plt.xscale('log')

plt.savefig('figures/opt/findLmaxM_CFHT.png', bbox_inches='tight')

plt.show()
