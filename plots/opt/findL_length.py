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


nls = [10, 20, 30, 50, 70, 90]
ref = 100

thmin = 0.5*1/60*np.pi/180
thmax = 100*1/60*np.pi/180
nth = 100

ths = np.linspace(np.log(thmin), np.log(thmax), nth)
ths = np.exp(ths)

thsarcmin = 180/np.pi*60*ths
thsdegree = thsarcmin/60

# create and collect data
create.findL_length(ref, clean=clean)
refsp = dat.get_xip_l_length(ref)
refsm = dat.get_xim_l_length(ref)


begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"), len(nls)))

i = 0
for l in nls:
    create.findL_length(l, clean=clean)
    xip = dat.get_xip_l_length(l)
    xim = dat.get_xim_l_length(l)

    # Plotting
    plt.figure(1)
    plt.plot(thsarcmin, [abs(xip[j] - refsp[j])/refsp[j] for j in range(len(refsp))], color=colors[i].rgb, label="$l$={0}" .format(l))
    plt.figure(3)
    plt.plot(thsarcmin, [abs(xim[j] - refsm[j])/refsm[j] for j in range(len(refsm))], color=colors[i].rgb, label="$l$={0}" .format(l))
    i += 1

plt.figure(1).set_size_inches((8, 8), forward=False)
plt.title("Relative difference varying number of $C_l$, lref={0}" .format(ref))
plt.xlabel("$\\theta$ (arcmin)")
plt.ylabel("$(\\xi^+-\\xi^+_{ref})/ \\xi_{ref}$")
plt.yscale('log')
plt.xscale('log')
plt.legend()
plt.savefig('figures/opt/finL_lengthP.png', bbox_inches='tight')

plt.figure(3).set_size_inches((8, 8), forward=False)
plt.title("Relative difference varying number of $C_l$, lref={0}" .format(ref))
plt.xlabel("$\\theta$ (arcmin)")
plt.ylabel("$(\\xi^--\\xi^-_{ref})/ \\xi_{ref}$")
plt.yscale('log')
plt.xscale('log')
plt.legend()
plt.savefig('figures/opt/findL_lengthM.png', bbox_inches='tight')

plt.show()
