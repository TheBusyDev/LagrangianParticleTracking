module NamelistModule
  use Numbers
  use RungeKuttaModule, only: FIRST_ORDER_FORWARD_EULER, &
                              SECOND_ORDER_MIDPOINT, SECOND_ORDER_HEUN, &
                              THIRD_ORDER_KUTTA, THIRD_ORDER_HEUN, &
                              FOURTH_ORDER_RK

  implicit none

contains

  ! Parse parameters from a given namelist file
  subroutine parse_parameters(filename, &
                              left, &
                              right, &
                              radius, &
                              n_points, &
                              n_particles, &
                              initial_time, &
                              final_time, &
                              delta_time, &
                              scheme, &
                              output_dir)
    ! The filename
    character(*), intent(in) :: filename
    ! Left and right endpoints of the square domain
    real(wp), intent(out) :: left, right
    ! Radius used to initialize the particles on a circular domain
    real(wp), intent(out) :: radius
    ! Number of points for each side of the square domain
    integer, intent(out) :: n_points
    ! Number of particles
    integer, intent(out) :: n_particles
    ! The initial time
    real(wp), intent(out) :: initial_time
    ! The final time
    real(wp), intent(out) :: final_time
    ! The time step
    real(wp), intent(out) :: delta_time
    ! The time-stepping scheme
    integer, intent(out) :: scheme
    ! The output directory
    character(*), intent(out) :: output_dir
    ! The time-stepping scheme (string version)
    character(64) :: scheme_str
    ! File unit
    integer :: fu

    ! Declare namelist
    namelist /config/ left, right, radius, n_points, n_particles, &
      initial_time, final_time, delta_time, scheme_str, output_dir

    ! Parse from file
    open (newunit=fu, file=trim(filename), status='old')
    read (fu, nml=config)
    close (fu)

    ! Select the time-stepping scheme
    select case (scheme_str)
    case ("FIRST_ORDER_FORWARD_EULER")
      scheme = FIRST_ORDER_FORWARD_EULER

    case ("SECOND_ORDER_MIDPOINT")
      scheme = SECOND_ORDER_MIDPOINT

    case ("SECOND_ORDER_HEUN")
      scheme = SECOND_ORDER_HEUN

    case ("THIRD_ORDER_KUTTA")
      scheme = THIRD_ORDER_KUTTA

    case ("THIRD_ORDER_HEUN")
      scheme = THIRD_ORDER_HEUN

    case ("FOURTH_ORDER_RK")
      scheme = FOURTH_ORDER_RK

    case default
      print *, "ERROR: Time-stepping method '"//scheme_str//"' not implemented."
      call exit(1)
    end select
  end subroutine parse_parameters

end module NamelistModule
