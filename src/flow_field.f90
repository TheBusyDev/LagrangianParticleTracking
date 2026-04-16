module FlowField
  implicit none

  ! This defines the TEMPLATE for any velocity field function
  abstract interface
    function flow_field_interface(x) result(v)
      ! Input: Position
      real, intent(in) :: x(2)
      ! Output: Velocity
      real :: v(2)
    end function flow_field_interface
  end interface

contains

  ! Evaluate the flow field and save it into an array
  function evaluate(flow_field, points) result(velocity)
    ! Flow field function
    procedure(flow_field_interface) :: flow_field
    ! Mesh points
    real, allocatable, intent(in) :: points(:, :, :)
    ! Flow field evaluation
    real, allocatable :: velocity(:, :, :)
    ! Counters
    integer i, j
    ! Size
    integer nx, ny

    nx = size(points, 2)
    ny = size(points, 3)

    allocate(velocity(2, nx, ny))

    do j = 1, ny
      do i = 1, nx
        velocity(:, i, j) = flow_field(points(:, i, j))
      end do
    end do
  end function evaluate


  ! A simple vortex
  function vortex(x) result(v)
    real, intent(in) :: x(2)
    real :: v(2)

    v(1) = -x(2) ! Velocity in x is -y
    v(2) = +x(1) ! Velocity in y is +x
  end function vortex

end module FlowField