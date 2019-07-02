import matplotlib.pyplot as plt
# import numpy as np

# # plt.subplot(2,1,1)
# plt.plot(x_axis, y_axis, color='blue', lw=2)

# logarithmic scale
plt.xscale('log')
plt.yscale('log')
# Add basey=2 to change base log

# labels
plt.xlabel('$k / h \ Mpc^{-1}$')
plt.ylabel('$\Delta^2 (k)$')

# collecting data
n = 11
data = open('data/power_hm.dat', "r")
lines = data.readlines()
data.close()

# parsing data
columns = []
params = []


for i in range(n-1):
    firstLine = lines[0].split('       ')
    params.append(float(firstLine[i+3]))


for i in range(n):
    column = []
    for j in range(1, len(lines)-1):
        line = lines[j].split('       ')[1]
        column.append(float(line.split('    ')[i]))
    line = lines[len(lines)-1].split('      ')[1]
    column.append(float(line.split('    ')[i]))

    columns.append(column)

# plotting
for i in range(1, n):
    plt.plot(columns[0], columns[i], label='{0}' .format(params[i-1]))
plt.legend()
plt.show()
