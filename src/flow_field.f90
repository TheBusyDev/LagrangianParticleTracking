module FlowFieldModule
  use VectorModule, only: VectorType
  use MeshModule, only: MeshType

  implicit none

  ! Flow field
  type, extends(VectorType) :: FlowFieldType
  end type FlowFieldType


  ! Flow field function
  abstract interface
    function flow_field_template(mesh) result(velocity)
      import MeshType, FlowFieldType
      ! Position
      type(MeshType), intent(in) :: mesh
      ! Velocity
      type(FlowFieldType) :: velocity
    end function flow_field_template
  end interface

contains

  ! 2D vortex
  function vortex(mesh) result(velocity)
    ! Position
    type(MeshType), intent(in) :: mesh
    ! Velocity
    type(FlowFieldType) :: velocity

    call velocity%init(mesh%n)

    velocity%x = -mesh%y ! Velocity in x is -y
    velocity%y = +mesh%x ! Velocity in y is +x
    velocity%z = 0.0
  end function vortex

end module FlowFieldModule