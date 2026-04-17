import numpy as np
import matplotlib.pyplot as plt

mesh = np.loadtxt("mesh.csv", delimiter=",")
mesh_x = mesh[:, 0]
mesh_y = mesh[:, 1]

flow_field = np.loadtxt("flow_field.csv", delimiter=",")
flow_field_x = flow_field[:, 0]
flow_field_y = flow_field[:, 1]

particles = np.loadtxt("particles_000000.csv", delimiter=",")
particles_x = particles[:, 0]
particles_y = particles[:, 1]

plt.figure()
plt.quiver(mesh_x,
           mesh_y,
           flow_field_x,
           flow_field_y,
           np.hypot(flow_field_x, flow_field_y),
           cmap='jet')
plt.scatter(particles_x, particles_y, c="black")
plt.colorbar()
plt.tight_layout()
plt.show()