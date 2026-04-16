program main
  use Mesh, only: square_mesh
  use FlowField, only: evaluate, vortex
  use IO, only: write_csv

  ! Left and right endpoints of the square domain
  real, parameter :: left = 0.0, right = 1.0
  ! Number of points
  integer, parameter :: np = 10
  ! Array of mesh points points
  real, allocatable :: points(:, :, :)
  ! Velocity field evaluated at mesh points
  real, allocatable :: velocity(:, :, :)
  !

  call square_mesh(left, right, np, points)

  print *, shape(points)

  velocity = evaluate(vortex, points)

  ! do j = 1, np
  !   do i = 1, np
  !     print *, velocity(1, i, j), velocity(2, i, j)
  !   end do
  ! end do

  call write_csv("points", points)
  call write_csv("velocity", points, velocity, ["vx, vy"])

end program main