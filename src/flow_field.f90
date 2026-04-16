module FlowField
  use Vector, only: VectorType

  implicit none

  ! This defines the template for any velocity field function
  abstract interface
    pure function flow_field_template(position) result(velocity)
      import VectorType
      ! Input: Position
      type(VectorType), intent(in) :: position
      ! Output: Velocity
      type(VectorType) :: velocity
    end function flow_field_template
  end interface

contains

  ! Evaluate the flow field at the selected points.
  function evaluate(flow_field, points) result(velocity)
    ! Flow field function
    procedure(flow_field_template) :: flow_field
    ! Array of points
    type(VectorType), allocatable, intent(in) :: points(:)
    ! Velocity field
    type(VectorType), allocatable :: velocity(:)
    ! Counter
    integer i

    allocate(velocity(size(points)))

    do i = 1, size(points)
      velocity(i) = flow_field(points(i))
    end do
  end function evaluate


  ! 2D vortex
  pure function vortex(position) result(velocity)
    ! Input: Position
    type(VectorType), intent(in) :: position
    ! Output: Velocity
    type(VectorType) :: velocity

    velocity%x = -position%y ! Velocity in x is -y
    velocity%y = +position%x ! Velocity in y is +x
    velocity%z = 0
  end function vortex

end module FlowField