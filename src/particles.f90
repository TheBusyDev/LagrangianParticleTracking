module ParticlesModule
  use VectorModule, only: VectorType

  implicit none

  ! Position of the particles
  type, extends(VectorType) :: ParticlesType
  end type ParticlesType


  ! Initialize position of the particles randomly on a 2D domain (either rectangular or square)
  ! (function overloading)
  interface init_random_2d
    procedure init_random_rectangle, init_random_square
  end interface init_random_2d


  private :: init_random_rectangle, init_random_square

  contains

  ! Initialize position of the particles randomly on a 2D rectangular domain
  subroutine init_random_rectangle(left, right, bottom, top, n_particles, particles)
    ! Left, right, bottom and top endpoints
    real, intent(in) :: left, right, bottom, top
    ! Number of particles
    integer, intent(in) :: n_particles
    ! Position of the particles
    class(ParticlesType), intent(out) :: particles

    ! Initialize particles
    call particles%init(n_particles)

    ! Initialize the position randomly
    call random_seed()
    ! Each element is initialized in the range [0, 1)
    call random_number(particles%x)
    call random_number(particles%y)

    particles%x = left + (right - left) * particles%x ! Rescale along x
    particles%y = bottom + (top - bottom) * particles%y ! Rescale along y
    particles%z = 0.0
  end subroutine init_random_rectangle


  ! Initialize position of the particles randomly on a 2D square domain
  subroutine init_random_square(left, right, n_particles, particles)
    ! Left and right endpoints
    real, intent(in) :: left, right
    ! Number of particles
    integer, intent(in) :: n_particles
    ! Position of the particles
    class(ParticlesType), intent(out) :: particles

    call init_random_rectangle(left, right, left, right, n_particles, particles)
  end subroutine init_random_square

end module ParticlesModule