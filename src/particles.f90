module ParticlesModule
  use Numbers
  use VectorModule, only: VectorType

  implicit none

contains

  ! Initialize position of the particles randomly on a 2D circular domain
  subroutine init_random_circle(max_radius, n_particles, particles)
    ! Maximum radius
    real(wp), intent(in) :: max_radius
    ! Number of particles
    integer, intent(in) :: n_particles
    ! Position of the particles
    class(VectorType), intent(out) :: particles
    ! Radial and angular position of the particles
    real(wp) :: r(n_particles), theta(n_particles)

    ! Initialize particles
    call particles%init(n_particles)

    ! Initialize the position randomly
    call random_seed()
    call random_number(r)
    call random_number(theta)

    r = r * max_radius
    theta = theta * (2.0_wp * PI)

    ! Convert polar coordinates into cartesian coordinates
    particles%x = r * cos(theta)
    particles%y = r * sin(theta)
    particles%z = 0.0_wp
  end subroutine init_random_circle


  ! Initialize position of the particles randomly on a 2D rectangular domain
  subroutine init_random_rectangle(left, right, bottom, top, n_particles, particles)
    ! Left, right, bottom and top endpoints
    real(wp), intent(in) :: left, right, bottom, top
    ! Number of particles
    integer, intent(in) :: n_particles
    ! Position of the particles
    class(VectorType), intent(out) :: particles

    ! Initialize particles
    call particles%init(n_particles)

    ! Initialize the position randomly
    call random_seed()
    ! Each element is initialized in the range [0, 1)
    call random_number(particles%x)
    call random_number(particles%y)

    particles%x = left + (right - left) * particles%x ! Rescale along x
    particles%y = bottom + (top - bottom) * particles%y ! Rescale along y
    particles%z = 0.0_wp
  end subroutine init_random_rectangle


  ! Initialize position of the particles randomly on a 2D square domain
  subroutine init_random_square(left, right, n_particles, particles)
    ! Left and right endpoints
    real(wp), intent(in) :: left, right
    ! Number of particles
    integer, intent(in) :: n_particles
    ! Position of the particles
    class(VectorType), intent(out) :: particles

    call init_random_rectangle(left, right, left, right, n_particles, particles)
  end subroutine init_random_square

end module ParticlesModule