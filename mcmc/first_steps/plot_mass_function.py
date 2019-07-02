import mf
import matplotlib.pyplot as plt
import numpy as np
from colour import Color
import os
# vprob=np.vectorize(mf.prob)


p_st=0.3
q_st=0.707
A_st=mf.normalisation(q_st, p_st)

p_range=np.linspace(-0.5,0.45, 11)
q_range=np.linspace(0.5, 1, 6)

p_range=np.insert(p_range, 0, p_st)
q_range=np.insert(q_range, 0, q_st)
x_axis=np.linspace(0.01, 5, 1000)


## colors for the plot
begin_color = Color("blue")
colors = list(begin_color.range_to(Color("green"),len(p_range)))


for q in q_range:

	index=0
	os.system("mkdir -p figures/q={0}" .format(q))

	for p in p_range:
		manager = plt.get_current_fig_manager()
		manager.resize(*manager.window.maxsize())
		plt.figure(1)
		norm=mf.normalisation(q, p)
		plots=[mf.prob(x, q, p)/norm for x in x_axis]
		plt.xscale('log')
		plt.yscale('log')
		plt.xlabel("$\\nu$")
		plt.ylabel("$f_{ST}(\\nu)$")
		if index==0:
			plt.plot(x_axis, plots, color=colors[index].rgb, linestyle='dashed' ,label="p={0}" .format(p))
		else:	
			plt.plot(x_axis, plots, color=colors[index].rgb ,label="p={0}" .format(p))
		plt.legend()
		
		plt.figure(2)
		plt.yscale('log')
		plt.xlabel("$\\nu$")
		plt.ylabel("$f_{ST}(\\nu)$")
		if index==0:
			plt.plot(x_axis, plots, color=colors[index].rgb, linestyle='dashed' ,label="p={0}" .format(p))
		else:	
			plt.plot(x_axis, plots, color=colors[index].rgb ,label="p={0}" .format(p))
		plt.legend()

		plt.figure(3)
		plt.xlabel("$\\nu$")
		plt.ylabel("$f_{ST}(\\nu)$")
		if index==0:
			plt.plot(x_axis, plots, color=colors[index].rgb, linestyle='dashed' ,label="p={0}" .format(p))
		else:	
			plt.plot(x_axis, plots, color=colors[index].rgb ,label="p={0}" .format(p))
		plt.legend()
		print('p='+str(p))
		index +=1

	plt.figure(1)
	plt.title("Mass function, q={0} fixed" .format(q))
	plt.figure(1).set_size_inches((16,9), forward=False)
	plt.savefig('figures/q={0}/q={0}loglog.png' .format(q))
	plt.clf()
	plt.figure(2)
	plt.title("Mass function, q={0} fixed" .format(q))
	plt.figure(2).set_size_inches((16,9), forward=False)
	plt.savefig('figures/q={0}/q={0}log.png' .format(q))
	plt.clf()
	plt.figure(3)
	plt.title("Mass function, q={0} fixed" .format(q))
	plt.figure(3).set_size_inches((16,9), forward=False)
	plt.savefig('figures/q={0}/q={0}classic.png' .format(q))
	plt.clf()

	print(q)


## exchange roles
## colors for the plot
begin_color = Color("red")
colors = list(begin_color.range_to(Color("blue"),len(q_range)))

for p in p_range:

	index=0
	os.system("mkdir -p figures/p={0}" .format(p))

	for q in q_range:
		plt.figure(1)
		norm=mf.normalisation(q, p)
		plots=[mf.prob(x, q, p)/norm for x in x_axis]
		plt.xscale('log')
		plt.yscale('log')
		plt.xlabel("$\\nu$")
		plt.ylabel("$f_{ST}(\\nu)$")
		if index == 0:
			plt.plot(x_axis, plots, color=colors[index].rgb, linestyle='dashed' ,label="q={0}" .format(q))
		else:
			plt.plot(x_axis, plots, color=colors[index].rgb ,label="q={0}" .format(q))
		plt.legend()

		plt.figure(2)
		plt.yscale('log')
		plt.xlabel("$\\nu$")
		plt.ylabel("$f_{ST}(\\nu)$")
		if index == 0:
			plt.plot(x_axis, plots, color=colors[index].rgb, linestyle='dashed' ,label="q={0}" .format(q))
		else:
			plt.plot(x_axis, plots, color=colors[index].rgb ,label="q={0}" .format(q))
		plt.legend()

		plt.figure(3)
		plt.xlabel("$\\nu$")
		plt.ylabel("$f_{ST}(\\nu)$")
		if index == 0:
			plt.plot(x_axis, plots, color=colors[index].rgb, linestyle='dashed' ,label="q={0}" .format(q))
		else:
			plt.plot(x_axis, plots, color=colors[index].rgb ,label="q={0}" .format(q))
		plt.legend()

		print('q='+str(q))
		index +=1
	plt.figure(1)
	plt.title("Mass function, p={0} fixed" .format(p))
	plt.figure(1).set_size_inches((16,9), forward=False)
	plt.savefig('figures/p={0}/p={0}loglog.png' .format(p))
	plt.clf()
	plt.figure(2)
	plt.title("Mass function, p={0} fixed" .format(p))
	plt.figure(2).set_size_inches((16,9), forward=False)
	plt.savefig('figures/p={0}/p={0}log.png' .format(p))
	plt.clf()
	plt.figure(3)
	plt.title("Mass function, p={0} fixed" .format(p))
	plt.figure(3).set_size_inches((16,9), forward=False)
	plt.savefig('figures/p={0}/p={0}classic.png' .format(p))
	plt.clf()
	print(p)
