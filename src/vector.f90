module VectorModule
  implicit none

  ! Array of 3D vectors, with x, y, z components.
  type :: VectorType
    ! Number of vectors stored in this class
    integer :: n = 0
    ! Components of the vector
    real, allocatable :: x(:), y(:), z(:)

  contains
    ! Initialization method
    procedure :: init => init_vector
    ! Multiply-add operation, i.e. this = this + s * v
    ! with s being a scalar and v another vector.
    procedure :: multiply_add => multiply_add_vector
    ! Write vector to a .csv file
    procedure :: write_to_csv => write_vector_to_csv
  end type VectorType


  private :: init_vector, multiply_add_vector, write_vector_to_csv

contains

  ! Initialize 3D vector and allocate components
  subroutine init_vector(this, n)
    ! The vector
    class(VectorType), intent(out) :: this
    ! Number of vectors
    integer, intent(in) :: n

    if (this%n .ne. n) then
      this%n = n

      if (allocated(this%x)) then
        deallocate(this%x)
      end if

      if (allocated(this%y)) then
        deallocate(this%y)
      end if

      if (allocated(this%z)) then
        deallocate(this%z)
      end if

    allocate(this%x(n))
    allocate(this%y(n))
    allocate(this%z(n))
  end if

    this%x = 0.0
    this%y = 0.0
    this%z = 0.0
  end subroutine init_vector


  ! Multiply-add operation, i.e. this = this + s * v
  ! with s being a scalar and v another vector.
  subroutine multiply_add_vector(this, s, v)
    ! The vector
    class(VectorType), intent(inout) :: this
    ! The scalar multiplying factor
    real, intent(in) :: s
    ! The other vector
    class(VectorType), intent(in) :: v

    this%x = this%x + s * v%x
    this%y = this%y + s * v%y
    this%z = this%z + s * v%z
  end subroutine multiply_add_vector


  ! Write 3D vector to .csv file
  subroutine write_vector_to_csv(this, filename, labels, timestep)
    ! The vector
    class(VectorType), intent(in) :: this
    ! The filename, without .csv extension
    character(*), intent(in) :: filename
    ! The labels used to write to .csv file
    character(*), intent(in):: labels(3)
    ! The time step (optional)
    integer, optional, intent(in) :: timestep
    ! The time step, in string format
    character(6) :: timestep_str
    ! File unit
    integer :: u_file
    ! Counter
    integer i

    ! Open the file (optionally, append time step to the filename)
    if (present(timestep)) then
      write(timestep_str, '(I6.6)') timestep
      open( &
        newunit=u_file, &
        file=filename//"_"//timestep_str//".csv", &
        status="replace", &
        action="write")
    else
      open( &
        newunit=u_file, &
        file=filename//".csv", &
        status="replace", &
        action="write")
    end if

    ! Write headers
    write(u_file, '(A)') "# "//labels(1)//", "//labels(2)//", "//labels(3)

    ! Write points and array
    do i = 1, this%n
      write(u_file, '(ES0.6, A, ES0.6, A, ES0.6)') &
        this%x(i), ", ", this%y(i), ", ", this%z(i)
    end do

    ! Close the file
    close(u_file)
  end subroutine write_vector_to_csv

end module VectorModule
