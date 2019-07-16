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

lmaxs = [1e3, 1e4, 5e4, 1e5, 5e5]
ref = 1e6

# Theta scale
thmin = 0.5*1/60*np.pi/180
thmax = 100*1/60*np.pi/180
nth = 100

ths = np.linspace(np.log(thmin), np.log(thmax), nth)
ths = np.exp(ths)

thsarcmin = 180/np.pi*60*ths
thsdegree = thsarcmin/60

# create reference data
create.findLmax(ref, clean=clean)

refsp = dat.get_xip(ref)
refsm = dat.get_xim(ref)

begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(lmaxs)))

i = 0
for l in lmaxs:
    # create data and collect it
    create.findLmax(l, clean=clean)
    xip = dat.get_xip(l)
    xim = dat.get_xim(l)

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
plt.savefig('figures/findmaxP.png', bbox_inches='tight')

plt.figure(3).set_size_inches((8, 8), forward=False)
plt.title("Variation of $\\xi_-$ changing modes number, lref={0}" .format(ref))
plt.xlabel("$\\theta$ (arcmin)")
plt.ylabel("$(\\xi_--\\xi_{ref})/ \\xi_{ref}$")
plt.yscale('log')
plt.xscale('log')

plt.savefig('figures/findLmaxM.png', bbox_inches='tight')

plt.show()
