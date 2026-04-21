program main
  use Numbers
  use NamelistModule, only: parse_parameters
  use VectorModule, only: VectorType
  use MeshModule, only: init_square_mesh
  use FlowFieldModule, only: vortex
  use ParticlesModule, only: init_random_circle
  use RungeKuttaModule, only: ExplicitRungeKuttaType, f_template

  implicit none

  ! Left and right endpoints of the square domain
  real(wp) :: left, right
  ! Radius used to initialize the particles on a circular domain
  real(wp) :: radius
  ! Number of points for each side of the square domain
  integer :: n_points
  ! Number of particles
  integer :: n_particles
  ! The initial time
  real(wp) :: initial_time
  ! The final time
  real(wp) :: final_time
  ! The time step
  real(wp) :: delta_time
  ! The time-stepping scheme
  integer :: scheme
  ! The current time
  real(wp) :: time
  ! The old time
  real(wp) :: old_time
  ! The number of the time step
  integer :: timestep
  ! Number of command line arguments
  integer :: n_args
  ! Namelist filename
  character(256) :: nml_filename
  ! Flow field function
  procedure(f_template), pointer :: flow_field_fun => vortex

  ! Array of mesh points
  type(VectorType) :: mesh
  ! Flow field evaluated at mesh points
  type(VectorType) :: flow_field
  ! Position of particles
  type(VectorType) :: particles
  ! Explicit RK method
  type(ExplicitRungeKuttaType) :: explicit_rk

  ! Read the namelist filename from the command line
  n_args = command_argument_count()

  if (n_args == 0) then
    print '(A)', "ERROR: you must specify the namelist filename in the arguments, e.g.:"
    print '(A)', "       >>> ./main config.nml"
    call exit(1)
  end if

  call get_command_argument(number=1, value=nml_filename)

  ! Read the variables from the namelist file
  print '(A)', "Parsing the variables from "//trim(nml_filename)//"..."
  call parse_parameters(nml_filename, &
                        left, &
                        right, &
                        radius, &
                        n_points, &
                        n_particles, &
                        initial_time, &
                        final_time, &
                        delta_time, &
                        scheme)

  time = initial_time
  old_time = initial_time
  timestep = 0

  ! Initialize LPT solver
  print '(A)', "Initializing the LPT solver..."

  ! Initialize mesh and position of particles
  call init_square_mesh(left, right, n_points, mesh)
  call init_random_circle(radius, n_particles, particles)

  ! Initialize RK scheme
  call explicit_rk%init(scheme, n_particles)

  ! Evaluate flow field
  call flow_field_fun(time, mesh, flow_field)

  ! Write mesh points, flow field and particles
  call mesh%write_to_csv("mesh", ["x", "y", "z"])
  call flow_field%write_to_csv("flow_field", ["vx", "vy", "vz"], timestep)
  call particles%write_to_csv("particles", ["x", "y", "z"], timestep)

  ! Time loop
  do while (time < (final_time - 0.5_wp * delta_time))
    ! Update time
    old_time = time
    time = time + delta_time
    timestep = timestep + 1
    print '(A, F0.5, A, I6.6)', "Time: ", time, " at timestep: ", timestep

    ! Update with explicit RK scheme
    call explicit_rk%update(particles, old_time, delta_time, flow_field_fun)

    ! Evaluate flow field
    call flow_field_fun(time, mesh, flow_field)

    ! Write results
    call flow_field%write_to_csv("flow_field", ["vx", "vy", "vz"], timestep)
    call particles%write_to_csv("particles", ["x", "y", "z"], timestep)
  end do

end program main