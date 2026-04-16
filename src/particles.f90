module ParticlesGenerator
  implicit none
  private :: init_random_rectangle, init_random_square

  ! Initialize position of the particles randomly on either rectangular or square mesh
  ! (function overloading).
  interface init_random
    module procedure init_random_rectangle, init_random_square
  end interface init_random

contains

  ! Initialize position of the particles randomly on a rectangular mesh
  subroutine init_random_rectangle(left, right, bottom, top, np, particles)
    ! Left, right, bottom and top endpoints
    real, intent(in) :: left, right, bottom, top
    ! Number of particles
    integer, intent(in) :: np
    ! Position of the particles
    real, allocatable, intent(out) :: particles(:, :)

    ! Allocate particles
    allocate(particles(2, np))

    ! Initialize randomly
    call random_seed()
    call random_number(particles) ! Each element is initialized in the range [0, 1)
    particles(1, :) = left + (right - left) * particles(1, :) ! Rescale along x
    particles(2, :) = bottom + (top - bottom) * particles(2, :) ! Rescale along y
  end subroutine init_random_rectangle


  ! Initialize position of the particles randomly on a square mesh
  subroutine init_random_square(left, right, np, particles)
    ! Left and right endpoints
    real, intent(in) :: left, right
    ! Number of particles
    integer, intent(in) :: np
    ! Position of the particles
    real, allocatable, intent(out) :: particles(:, :)

    call init_random_rectangle(left, right, left, right, np, particles)
  end subroutine init_random_square

end module ParticlesGenerator