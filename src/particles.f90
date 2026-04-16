module Particles
  use Vector, only: VectorType

  implicit none

  private :: init_random_rectangle, init_random_square

  ! Initialize position of the particles randomly on 2D mesh (either rectangular or square)
  ! (function overloading)
  interface init_random
    module procedure init_random_rectangle, init_random_square
  end interface init_random

contains

  ! Initialize position of the particles randomly on a 2D rectangular mesh
  subroutine init_random_rectangle(left, right, bottom, top, np, particles)
    ! Left, right, bottom and top endpoints
    real, intent(in) :: left, right, bottom, top
    ! Number of particles
    integer, intent(in) :: np
    ! Position of the particles
    type(VectorType), allocatable, intent(out) :: particles(:)

    ! Allocate particles
    allocate(particles(np))

    ! Initialize randomly
    call random_seed()
    ! Each element is initialized in the range [0, 1)
    call random_number(particles%x)
    call random_number(particles%y)

    particles%x = left + (right - left) * particles%x ! Rescale along x
    particles%y = bottom + (top - bottom) * particles%y ! Rescale along y
    particles%z = 0.0
  end subroutine init_random_rectangle


  ! Initialize position of the particles randomly on a 2D square mesh
  subroutine init_random_square(left, right, np, particles)
    ! Left and right endpoints
    real, intent(in) :: left, right
    ! Number of particles
    integer, intent(in) :: np
    ! Position of the particles
    type(VectorType), allocatable, intent(out) :: particles(:)

    call init_random_rectangle(left, right, left, right, np, particles)
  end subroutine init_random_square

end module Particles