import numpy as np
import scipy as cp
from scipy.special import gamma
import matplotlib.pyplot as plt


def myfunc(p):
	return 2**(-p)*gamma(0.5-p)

myfuncv=np.vectorize(myfunc)

p_axis=np.arange(0, 0.45, 0.01)

plt.plot(p_axis, myfuncv(p_axis))
plt.show()