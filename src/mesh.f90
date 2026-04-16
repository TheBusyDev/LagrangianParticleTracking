module Mesh
  implicit none

contains

  ! Create a square mesh, with the given left and right endpoints and the number of nodes.
  subroutine square_mesh(left, right, np, points)
    ! Left and right endpoints
    real, intent(in) :: left, right
    ! Number of points
    integer, intent(in) :: np
    ! Points array
    real, allocatable, intent(out) :: points(:, :, :)
    ! Counters
    integer :: i, j
    ! Space discretization
    real :: dx
    ! Coordinate array
    real :: coord(np)

    ! Allocate points array
    allocate(points(2, np, np))

    ! Define coordinate array
    dx = (right - left) / (np - 1)
    coord = [(left + (i - 1) * dx, i = 1, np)]

    ! Initialize arrays of points (exploit column-major ordering)
    do j = 1, np
      points(1, :, j) = coord
      points(2, :, j) = coord(j)
    end do
  end subroutine square_mesh

end module Mesh