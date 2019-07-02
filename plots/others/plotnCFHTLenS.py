import matplotlib.pyplot as plt
import numpy as np

z1 = 0.7
z2 = 1.2
a = 1.50
b = 0.32
c = 0.20
d = 0.46
norm = 1.0129840620118542


def nzCFTHLens(z):
    return (a*np.exp(-((z-z1)/b)**2)+c*np.exp(-((z-z2)/d)**2))/norm


z_axis = np.arange(0, 3, 0.01)
a_axis = np.arange(0.25, 1.0, 0.01)

yz = [nzCFTHLens(z) for z in z_axis]
ya = [nzCFTHLens(1/a - 1) for a in a_axis]

plt.figure(1)
plt.title('Density function of CFHTLenS survey')
plt.plot(z_axis, yz)
plt.xlabel('z')
plt.ylabel('$n$')

plt.figure(2)
plt.title('Density function of CFHTLenS survey')
plt.plot(a_axis, ya)
plt.xlabel('a')
plt.ylabel('$n$')

plt.show()
