module VectorModule
  implicit none

  ! Array of 3D vectors, with x, y, z components.
  type :: VectorType
    ! Number of vectors stored in this class
    integer, private :: n_vectors = 0
    ! Components of the vector
    real, allocatable :: x(:), y(:), z(:)

  contains
    ! Initialization method
    procedure :: init => init_vector
    ! Save vector to a .csv file (private method)
    procedure, private :: write_vector_to_csv
    ! Write vector to a .csv file
    generic :: write_to_csv => write_vector_to_csv
  end type VectorType


  ! Array of mesh points
  type, extends(VectorType) :: MeshType
  contains
    ! Save mesh to a .csv file (private method)
    procedure, private :: write_mesh_to_csv
    ! Overloading of write_to_csv
    generic :: write_to_csv => write_mesh_to_csv
  end type MeshType


  ! Flow field
  type, extends(VectorType) :: FlowFieldType
  contains
    ! Save flow field to a .csv file (private method)
    procedure, private :: write_flow_field_to_csv
    ! Overloading of write_to_csv
    generic :: write_to_csv => write_flow_field_to_csv
  end type FlowFieldType


  ! Position of the particles
  type, extends(VectorType) :: ParticlesType
  contains
    ! Save flow field to a .csv file (private method)
    procedure, private :: write_particles_to_csv
    ! Overloading of write_to_csv
    generic :: write_to_csv => write_particles_to_csv
  end type ParticlesType


  private :: &
    init_vector, &
    write_vector_to_csv, &
    write_mesh_to_csv, &
    write_flow_field_to_csv, &
    write_particles_to_csv

contains

  ! Initialize 3D vector and allocate components
  subroutine init_vector(this, n_vectors)
    ! The vector
    class(VectorType), intent(inout) :: this
    ! Number of points
    integer, intent(in) :: n_vectors

    this%n_vectors = n_vectors

    allocate(this%x(n_vectors))
    allocate(this%y(n_vectors))
    allocate(this%z(n_vectors))

    this%x = 0.0
    this%y = 0.0
    this%z = 0.0
  end subroutine init_vector


  ! Write 3D vector to .csv file
  subroutine write_vector_to_csv(this, filename, labels)
    ! The vector
    class(VectorType), intent(inout) :: this
    ! The filename, without .csv extension
    character(*), intent(in) :: filename
    ! The labels used to write to .csv file
    character(*), intent(in):: labels(3)
    ! File unit
    integer :: u_file
    ! Counter
    integer i

    ! Open the file
    open(newunit=u_file, file=filename//'.csv', status='replace', action='write')

    ! Write headers
    write(u_file, '(A)') "# "//labels(1)//", "//labels(2)//", "//labels(3)

    ! Write points and array
    do i = 1, this%n_vectors
      write(u_file, '(ES0.6, A, ES0.6, A, ES0.6)') &
        this%x(i), ", ", this%y(i), ", ", this%z(i)
    end do

    ! Close the file
    close(u_file)
  end subroutine write_vector_to_csv


  ! Write mesh points to 'points.csv' file
  subroutine write_mesh_to_csv(this)
    ! The mesh
    class(MeshType), intent(inout) :: this
    call write_vector_to_csv(this, "mesh", ["x", "y", "z"])
  end subroutine write_mesh_to_csv


  ! Write flow field to 'flow_field.csv' file
  subroutine write_flow_field_to_csv(this)
    ! The flow field
    class(FlowFieldType), intent(inout) :: this
    call write_vector_to_csv(this, "flow_field", ["vx", "vy", "vz"])
  end subroutine write_flow_field_to_csv


  ! Write particles to 'particles_XXXXXX.csv' file
  subroutine write_particles_to_csv(this, timestep)
    ! The particles
    class(ParticlesType), intent(inout) :: this
    ! The time step
    integer, intent(in) :: timestep
    ! The time step, in string format
    character(6) :: timestep_str

    write(timestep_str, '(I6.6)') timestep
    call write_vector_to_csv(this, "particles_"//timestep_str, ["x", "y", "z"])
  end subroutine write_particles_to_csv

end module VectorModule
