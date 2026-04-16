program main
  use Mesh, only: square_mesh
  use FlowField, only: evaluate, vortex
  use ParticlesGenerator, only: init_random
  use IO, only: write_csv

  implicit none

  ! Left and right endpoints of the square domain
  real, parameter :: left = 0.0, right = 1.0
  ! Number of points
  integer, parameter :: n_points = 10
  ! Number of particles
  integer, parameter :: n_particles = 1000
  ! Array of mesh points
  real, allocatable :: points(:, :, :)
  ! Velocity field evaluated at mesh points
  real, allocatable :: velocity(:, :, :)
  ! Position of particles
  real, allocatable :: particles(:, :)

  call square_mesh(left, right, n_points, points)
  call init_random(left, right, n_particles, particles)

  velocity = evaluate(vortex, points)

  ! do j = 1, np
  !   do i = 1, np
  !     print *, velocity(1, i, j), velocity(2, i, j)
  !   end do
  ! end do

  call write_csv("points", points)
  call write_csv("velocity", points, velocity, ["vx, vy"])

end program main