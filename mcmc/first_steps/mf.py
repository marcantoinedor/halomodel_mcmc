import numpy as np
import scipy as cp
from scipy.special import gamma
def normalisation(q,p):
	'''
		float float -> float
	'''
	return ((cp.sqrt(np.pi/(2*q))+cp.sqrt(q)*2**(-p)*gamma(0.5-p))**(-1))

def prob(x, q, p):
	'''
		float, float, float -> float
	'''
	infactor=1+(q*x*x)**(-p)
	return infactor*cp.exp(-q*x*x/2)

def lnprob(x, q, p):
	'''
		float, float, float -> float
	'''
	inlog=1+(q*x*x)**(-p)
	return -q*x*x/2+np.log(inlog)

