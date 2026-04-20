module FlowFieldModule
  use Numbers
  use VectorModule, only: VectorType

  implicit none

  ! Flow field
  type, extends(VectorType) :: FlowFieldType
  end type FlowFieldType

contains

  ! 2D vortex
  subroutine vortex(time, points, flow_field)
    ! Time
    real(wp), intent(in) :: time
    ! Position
    class(VectorType), intent(in) :: points
    ! Flow field
    class(VectorType), intent(out) :: flow_field

    ! Silence compiler warnings
    associate(dummy => time)
    end associate

    ! Initialize flow field
    call flow_field%init(points%n)

    flow_field%x = -points%y ! Velocity in x is -y
    flow_field%y = +points%x ! Velocity in y is +x
    flow_field%z = 0.0_wp
  end subroutine vortex

end module FlowFieldModule