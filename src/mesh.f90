module Mesh
  use Vector, only: VectorType

  implicit none

contains

  ! Create a square mesh, with the given left and right endpoints and the number of nodes.
  ! Points are saved with a column-major ordering.
  subroutine square_mesh(left, right, np, points)
    ! Left and right endpoints
    real, intent(in) :: left, right
    ! Number of points for each side of the square domain
    integer, intent(in) :: np
    ! Array of points
    type(VectorType), allocatable, intent(out) :: points(:)
    ! Counters
    integer :: i, j
    ! Space discretization
    real :: dx

    ! Allocate points array
    allocate(points(np * np))

    ! Define coordinate array
    dx = (right - left) / (np - 1)

    ! Initialize array of points (exploit column-major ordering)
    do j = 1, np
      do i = 1, np
        points((j - 1) * np + i)%x = left + (i - 1) * dx
        points((j - 1) * np + i)%y = left + (j - 1) * dx
        points((j - 1) * np + i)%z = 0
      end do
    end do
  end subroutine square_mesh

end module Mesh