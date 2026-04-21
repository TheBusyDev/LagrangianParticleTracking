import glob
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


OUTPUT_DIR = "output"
DELTA_TIME = 0.1 # Physical timestep


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
  mesh = np.loadtxt(f"{OUTPUT_DIR}/mesh.csv", delimiter=",")

  # Load flow field and particles.
  flow_fields = load_timesteps(f"{OUTPUT_DIR}/flow_field")
  particles = load_timesteps(f"{OUTPUT_DIR}/particles")

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
  fig.colorbar(quiver_plot, label="Flow field magnitude")

  scatter_plot = ax.scatter(particles[0][:, 0],
                            particles[0][:, 1],
                            c="black",
                            s=10)
  ax.set_aspect('equal')
  fig.tight_layout()

  # Plot one single frame.
  def plot_frame(frame: int) -> None:
    quiver_plot.set_UVC(flow_fields[frame][:, 0],
                        flow_fields[frame][:, 1],
                        np.linalg.norm(flow_fields[frame], axis=1))

    scatter_plot.set_offsets(particles[frame][:, :2])

  animation = FuncAnimation(fig=fig,
                            func=plot_frame,
                            frames=n_timesteps,
                            interval=DELTA_TIME*1000)
  animation.save("animation.mp4", dpi=200)
  plt.show()

  exit(0)