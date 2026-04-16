module IO
  implicit none

contains

  ! Write array into the specified .csv file
  subroutine write_csv(filename, points, array, labels)
    ! The filename, without the .csv extension
    character(*), intent(in) :: filename
    ! The mesh points
    real, intent(in) :: points(:, :, :)
    ! The array to be saved
    real, intent(in), optional :: array(:, :, :)
    ! The labels
    character(*), intent(in), optional :: labels(:)
    ! Counters
    integer i, j
    ! Size
    integer nx, ny
    ! File unit
    integer :: u_file
    ! Write array?
    logical :: write_array
    write_array = present(array) .and. present(labels)

    nx = size(points, 2)
    ny = size(points, 3)

    ! Open the file
    open(newunit=u_file, file=filename//'.csv', status='replace', action='write')

    ! Write headers
    write(u_file, '(A)', advance='no') "# x, y"

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
      do j = 1, ny
        do i = 1, nx
          write(u_file, '(ES0.6, A)', advance='no') points(1, i, j), ", "
          write(u_file, '(ES0.6, A)', advance='no') points(2, i, j), ", "
          write(u_file, '(ES0.6, A)', advance='no') array(1, i, j), ", "
          write(u_file, '(ES0.6)') array(2, i, j)
        end do
      end do
    else
      ! Write points only
      do j = 1, ny
        do i = 1, nx
          write(u_file, '(ES0.6, A)', advance='no') points(1, i, j), ", "
          write(u_file, '(ES0.6)') points(2, i, j)
        end do
      end do
    end if

    ! Close the file
    close(u_file)
  end subroutine write_csv

end module IO