program main
  use MeshModule, only: MeshType, init_square_mesh
  use FlowFieldModule, only: FlowFieldType, flow_field_template, vortex
  use ParticlesModule, only: ParticlesType, init_random_2d

  implicit none

  ! Left and right endpoints of the square domain
  real, parameter :: left = -1.0, right = 1.0
  ! Number of points
  integer, parameter :: n_points = 10
  ! Number of particles
  integer, parameter :: n_particles = 1000
  ! Flow field function
  procedure(flow_field_template), pointer :: flow_field_fun => vortex

  ! Array of mesh points
  type(MeshType) :: mesh
  ! Flow field evaluated at mesh points
  type(FlowFieldType) :: flow_field
  ! Position of particles
  type(ParticlesType) :: particles

  ! Initialize mesh and position of particles
  call init_square_mesh(left, right, n_points, mesh)
  call init_random_2d(left, right, n_particles, particles)

  ! Evaluate flow field
  flow_field = flow_field_fun(mesh)

  ! Write mesh points
  call mesh%write_to_csv("mesh", ["x", "y", "z"])
  call flow_field%write_to_csv("flow_field", ["vx", "vy", "vz"])
  call particles%write_to_csv("particles", ["x", "y", "z"], timestep=0)

end program main