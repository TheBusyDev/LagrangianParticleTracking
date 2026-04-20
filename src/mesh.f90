module MeshModule
  use Kinds
  use VectorModule, only: VectorType

  implicit none

  ! Array of mesh points
  type, extends(VectorType) :: MeshType
  end type MeshType

contains

  ! Create a square mesh, with the given left and right endpoints and the number of nodes
  ! (points are saved with a column-major ordering)
  subroutine init_square_mesh(left, right, n_points, mesh)
    ! Left and right endpoints
    real(wp), intent(in) :: left, right
    ! Number of points for each side of the square domain
    integer, intent(in) :: n_points
    ! Array of points
    class(MeshType), intent(out) :: mesh
    ! Counters
    integer :: i, j
    ! Array of coordinates
    real(wp) :: coord(n_points)

    ! Initialize mesh
    call mesh%init(n_points * n_points)

    ! Define coordinate array
    coord = [(left + i * (right - left) / (n_points - 1), i = 0, n_points - 1)]

    ! Initialize array of points (exploit column-major ordering)
    do j = 1, n_points
      do i = 1, n_points
        mesh%x((j - 1) * n_points + i) = coord(i)
        mesh%y((j - 1) * n_points + i) = coord(j)
      end do
    end do

    mesh%z = 0.0_wp
  end subroutine init_square_mesh

end module MeshModule