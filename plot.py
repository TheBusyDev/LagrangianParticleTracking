import numpy as np
import matplotlib.pyplot as plt

data = np.loadtxt("velocity.csv", delimiter=",")
# Points
px = data[:, 0]
py = data[:, 1]
pz = data[:, 2]
# Velocity
vx = data[:, 3]
vy = data[:, 4]
vz = data[:, 5]

plt.figure()
plt.quiver(px, py, vx, vy, np.hypot(vx, vy), cmap='jet')
plt.colorbar()
plt.tight_layout()
plt.show()