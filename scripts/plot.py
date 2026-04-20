import glob
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


def load_timesteps(basename: str) -> list[np.ndarray]:
  '''
  Load data from basename_XXXXXX.csv files.
  Return a list of numpy arrays, one for each timestep.
  '''
  filenames = glob.glob(f"{basename}_*.csv")
  result = list()

  for filename in filenames:
    timestep = filename.removeprefix(f"{basename}_").removesuffix(".csv")
    timestep = int(timestep)
    data = np.loadtxt(filename, delimiter=",")
    result.append(data)

  return result


# MAIN PROGRAM.
if __name__ == "__main__":
  # Load mesh.
  mesh = np.loadtxt("mesh.csv", delimiter=",")

  # Load flow field and particles.
  flow_fields = load_timesteps("flow_field")
  particles = load_timesteps("particles")

  n_timesteps = len(flow_fields)
  assert len(particles) == n_timesteps

  # Plot.
  fig = plt.gcf()
  ax = plt.gca()

  # Initialize plots.
  quiver_plot = ax.quiver(mesh[:, 0],
                          mesh[:, 1],
                          flow_fields[0][:, 0],
                          flow_fields[0][:, 1],
                          np.linalg.norm(flow_fields[0], axis=1),
                          cmap="jet")
  fig.colorbar(quiver_plot)

  scatter_plot = ax.scatter(particles[0][:, 0],
                              particles[0][:, 1],
                              c="black")

  ax.set_aspect('equal')
  fig.tight_layout()

  # Plot one single frame.
  def plot_frame(frame: int) -> None:
    i = frame % n_timesteps

    quiver_plot.set_UVC(flow_fields[i][:, 0],
                        flow_fields[i][:, 1],
                        np.linalg.norm(flow_fields[i], axis=1))

    scatter_plot.set_offsets(particles[i][:, :2])

  animation = FuncAnimation(fig, plot_frame)
  plt.show()
  exit(0)