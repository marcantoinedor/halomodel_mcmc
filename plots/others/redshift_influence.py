import matplotlib.pyplot as plt
import os
import time
import get

n=11
term='1h'
scales=[i*0.1 for i in range(1, 11)]
ref=[]
os.system("python3 utils/create_data.py 1e17")

scales=[0.1*i for i in range(1, 11)]
index_scale=0
mmaxs=[]
for scale in scales:
	mmax=10**17
	exp=17
	mmax_sci='1e17'

	ref = get.get_column(term, mmax_sci, scale)

	## Comparing
	# get data
	column = get.get_column(term, mmax_sci, scale)

	mean=max([abs(column[i] - ref[i])/ref[i] for i in range(80)])
	iterations=0
	eps=0.9*mmax
	inf=True
	imp=10

	## exp decrease

	while inf and exp > 8:
		eps=0.9*mmax
		exp-=1
		mmax_sci='1e'+ str(exp)
		mmax-=eps
		up=mmax*10

		# update data
		column=get.get_column(term, mmax_sci, scale)

		mean=max([abs(column[i] - ref[i])/ref[i] for i in range(80)])
		if mean > 3*10**(-2):
			inf=False
			up=10
			down=1
			eps=5
			med=(up+down)/2
			mmax_sci='5.5e'+str(exp)

			# update data
			column=get.get_column(term, mmax_sci, scale)

			mean=max([abs(column[i] - ref[i])/ref[i] for i in range(80)])

	## dichotomial decrease
	# print('exp' + str(exp))
	while((mean > 3*10**(-2)) or (mean < 1*10**(-2)) and exp > 8):
		# print(mean)
		# print(up)
		# print(down)
		# time.sleep(2)
		if mean > 2*10**(-2):
			down=down+eps
			eps=eps/2
			imp=down+eps
			mmax_sci=str(imp)[:7]+'e'+str(exp)
			print('up '+mmax_sci)
		else:
			up=up-eps
			eps=eps/2
			imp=up-eps
			mmax_sci=str(imp)[:7]+'e'+str(exp)
			print('down '+mmax_sci)

		# update data
		column=get.get_column(term, mmax_sci, scale)

		iterations+=1
		mean=max([abs(column[i] - ref[i])/ref[i] for i in range(80)])

	# print(iterations)
	mmaxs.append(float(mmax_sci.split('e')[0])*10**exp)
	index_scale=index_scale+1

plt.yscale('log')
plt.xlabel('$a$ (Scale factor)')
plt.ylabel("$M_{max}$")
plt.title('Influence of redshift on the upper limit to reach $1\%$, {0} term' .format(term))
plt.plot(scales, mmaxs, 'ro')
plt.show()