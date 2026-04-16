program main
  use Vector, only: VectorType
  use Mesh, only: square_mesh
  use FlowField, only: flow_field_template, evaluate, vortex
  use Particles, only: init_random
  use IO, only: write_csv, write_velocity_to_csv

  implicit none

  ! Left and right endpoints of the square domain
  real, parameter :: left = -1.0, right = 1.0
  ! Number of points
  integer, parameter :: n_points = 10
  ! Number of particles
  integer, parameter :: n_particles = 1000
  ! Flow field function
  procedure(flow_field_template), pointer :: flow_field => vortex

  ! Array of mesh points
  type(VectorType), allocatable :: points(:)
  ! Velocity field evaluated at mesh points
  type(VectorType), allocatable :: velocity(:)
  ! Position of particles
  type(VectorType), allocatable :: particles_position(:)

  ! Initialize mesh and position of particles
  call square_mesh(left, right, n_points, points)
  call init_random(left, right, n_particles, particles_position)

  ! Evaluate velocity field
  velocity = evaluate(flow_field, points)

  call write_csv("points", points)
  call write_velocity_to_csv("velocity", points, velocity)

end program main