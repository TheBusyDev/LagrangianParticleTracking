module Numbers
  ! Define Working Precision (wp)
  use, intrinsic :: iso_fortran_env, only: wp => real64

  implicit none

  ! Pi constant
  real(wp), parameter :: PI = 4.0_wp * atan(1.0_wp)

end module Numbers
