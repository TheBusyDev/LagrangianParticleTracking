module FlowFieldModule
  use VectorModule, only: VectorType
  use MeshModule, only: MeshType

  implicit none

  ! Flow field
  type, extends(VectorType) :: FlowFieldType
  end type FlowFieldType


  ! Flow field function
  abstract interface
    subroutine flow_field_template(mesh, flow_field)
      import MeshType, FlowFieldType
      ! Position
      type(MeshType), intent(in) :: mesh
      ! Flow field
      type(FlowFieldType), intent(out) :: flow_field
    end subroutine flow_field_template
  end interface

contains

  ! 2D vortex
  subroutine vortex(mesh, flow_field)
    ! Position
    type(MeshType), intent(in) :: mesh
    ! Flow field
    type(FlowFieldType), intent(out) :: flow_field

    ! Initialize flow field
    call flow_field%init(mesh%n)

    flow_field%x = -mesh%y ! Velocity in x is -y
    flow_field%y = +mesh%x ! Velocity in y is +x
    flow_field%z = 0.0
  end subroutine vortex

end module FlowFieldModule