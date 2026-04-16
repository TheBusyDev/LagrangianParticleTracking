import numpy as np
import matplotlib.pyplot as plt

data = np.loadtxt("velocity.csv", delimiter=",")
points_x = data[:, 0]
points_y = data[:, 1]
vx = data[:, 2]
vy = data[:, 3]

plt.figure()
plt.quiver(points_x, points_y, vx, vy, np.hypot(vx, vy), cmap='jet')
plt.colorbar()
plt.tight_layout()
plt.show()