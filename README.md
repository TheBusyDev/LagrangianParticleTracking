# Lagrangian Particle Tracking in FORTRAN

This a simple **Lagrangian Particle Tracking** (LPT) using explicit Runge-Kutta
time stepping, written in *FORTRAN 2008*. 🤓

![a beautiful animation](images/animation.gif)

## How to build 🧑‍💻

The entire project can be build using `cmake`, in either `DEBUG` mode:
```
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug
make all
```
or `RELEASE` mode:
```
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make all
```
These commands produce an executable `main`.

## How to run 🚀

After building the project, the executable must be called by passing the
*namelist* file to declare relevant parameter, e.g.:
```
./main ./examples/config.nml
```
> **NOTE:** [`config.nml`](examples/config.nml) is an example namelist file to
  be used in this project.

Equivalently, one can execute:
```
make run # equivalent to: ./main ./examples/config.nml
```
This will produce some `.csv` files in the `output` folder, containing the flow
field and the position of the particles at each timestep.

## How to plot 🌈

The plot script [plot.py](scripts/plot.py) is written in *Python* and relies on
`numpy` and `matplotlib`. Setting up a virtual environment is highly suggested,
e.g. on Linux:
```
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```
The *beautiful* animation [animation.gif](images/animation.gif) is obtained by
calling in the build directory:
```
cd build # or move to your custom build directory
make plot # equivalent to: python ./scripts/plot.py
```

## Code base 🧐

The code base is composed of the following modules:
- [`VectorType`](src/vector.f90) defines a class `VectorType`, used to store
  an array of 3D vectors and save them into `.csv` files;
- [`RungeKuttaModule`](src/runge_kutta.f90) implements the explicit Runge-Kutta
  method, with some useful enumerators to select the scheme (as listed
  [here](https://en.wikipedia.org/wiki/List_of_Runge–Kutta_methods));
- [`FlowFieldModule`](src/flow_field.f90) contains the known flow field
  functions;
- [`MeshModule`](src/mesh.f90) collects functions to initialize the mesh, used
  to evaluate the flow field;
- [`ParticlesModule`](src/particles.f90) contains functions to initialize the
  position of the particles;
- [`NamelistModule`](src/namelist.f90) is used to parse parameters from namelist
  files - an example is provided in [`config.nml`](examples/config.nml);
- [`Numbers`](src/numbers.f90) defines the working precision and other numeric
  constants.

## Bonus: format your code automatically

The code can be formatted automatically by using
[`fprettify`](https://github.com/fortran-lang/fprettify), which can be installed
through `pip`:
```
pip install fprettify
```
The automatic formatting is obtained by calling (at the top of the repository):
```
fprettify --config-file .fprettify.rc src/*.f90
```

