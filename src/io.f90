module IO
  use Vector, only: VectorType

  implicit none

contains

  ! Write array into the specified .csv file
  subroutine write_csv(filename, points, array, labels)
    ! The filename, without the .csv extension
    character(*), intent(in) :: filename
    ! The mesh points
    type(VectorType), intent(in) :: points(:)
    ! The array to be saved
    type(VectorType), intent(in), optional :: array(:)
    ! The labels
    character(*), intent(in), optional :: labels(:)
    ! Counter
    integer i
    ! File unit
    integer :: u_file
    ! Write array?
    logical :: write_array
    write_array = present(array) .and. present(labels)

    ! Open the file
    open(newunit=u_file, file=filename//'.csv', status='replace', action='write')

    ! Write headers
    write(u_file, '(A)', advance='no') "# x, y, z"

    if (write_array) then
      write(u_file, '(A)', advance='no') ", "

      do i = 1, size(labels) - 1
        write(u_file, '(A)', advance='no') labels(i)//", "
      end do

      write(u_file, '(A)', advance='no') labels(size(labels))
    end if

    write(u_file, *) ! Write a new line

    ! Write points and array
    if (write_array) then
      ! Write points and array content
      do i = 1, size(points)
        write(u_file, '(ES0.6, A, ES0.6, A, ES0.6, A)', advance='no') &
          points(i)%x, ", ", points(i)%y, ", ", points(i)%z, ", "

        write(u_file, '(ES0.6, A, ES0.6, A, ES0.6)') &
          array(i)%x, ", ", array(i)%y, ", ", array(i)%z
      end do
    else
      ! Write points only
      do i = 1, size(points)
        write(u_file, '(ES0.6, A, ES0.6, A, ES0.6)') &
          points(i)%x, ", ", points(i)%y, ", ", points(i)%z
      end do
    end if

    ! Close the file
    close(u_file)
  end subroutine write_csv


  ! Write the velocity field into the .csv file.
  subroutine write_velocity_to_csv(filename, points, velocity)
    ! The filename, without the .csv extension
    character(*), intent(in) :: filename
    ! The mesh points
    type(VectorType), intent(in) :: points(:)
    ! The velocity field
    type(VectorType), intent(in) :: velocity(:)

    call write_csv(filename, points, velocity, ["vx", "vy", "vz"])
  end subroutine write_velocity_to_csv

end module IO