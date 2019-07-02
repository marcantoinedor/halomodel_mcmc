import numpy as np
import scipy as cp
from scipy.special import gamma
def normalisation(q,p):
	'''
		float float -> float
	'''
	return (1.0/(cp.sqrt(np.pi/(2*q))+1.0/cp.sqrt(q)*2**(-p-0.5)*gamma(0.5-p)))

def unprob(x, q, p):
	'''
		float, float, float -> float
	'''
	infactor=1+(q*x*x)**(-p)
	return infactor*cp.exp(-q*x*x/2)

def lnunprob(x, q, p):
	'''
		float, float, float -> float
	'''
	inlog=1+(q*x*x)**(-p)
	return -q*x*x/2+np.log(inlog)

def unprobv(x, q, p):
	'''
		ndarray, float, float -> ndarray
	'''
	infactor=1+(q*x*x)**(-p)
	return infactor*np.exp(-q*x*x/2)