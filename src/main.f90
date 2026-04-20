program main
  use Kinds
  use MeshModule, only: MeshType, init_square_mesh
  use FlowFieldModule, only: FlowFieldType, vortex
  use ParticlesModule, only: ParticlesType, init_random_2d
  use RungeKuttaModule, only: ExplicitRungeKuttaType, f_template, FORWARD_EULER

  implicit none

  ! Left and right endpoints of the square domain
  real(wp), parameter :: left = -1.0_wp, right = 1.0_wp
  ! Number of points for each side of the square domain
  integer, parameter :: n_points = 10
  ! Number of particles
  integer, parameter :: n_particles = 1000
  ! The initial time
  real(wp), parameter :: initial_time = 0.0_wp
  ! The final time
  real(wp), parameter :: final_time = 1.0_wp
  ! The time step
  real(wp), parameter :: delta_time = 0.01_wp
  ! The current time
  real(wp) :: time = initial_time
  ! The old time
  real(wp) :: old_time = initial_time
  ! The number of the time step
  integer :: timestep = 0
  ! Flow field function
  procedure(f_template), pointer :: flow_field_fun => vortex

  ! Array of mesh points
  type(MeshType) :: mesh
  ! Flow field evaluated at mesh points
  type(FlowFieldType) :: flow_field
  ! Position of particles
  type(ParticlesType) :: particles
  ! Explicit RK method
  type(ExplicitRungeKuttaType) :: explicit_rk

  print '(A)', "Initializing the LPT solver..."

  ! Initialize mesh and position of particles
  call init_square_mesh(left, right, n_points, mesh)
  call init_random_2d(left, right, n_particles, particles)

  ! Initialize RK scheme
  call explicit_rk%init(FORWARD_EULER, n_particles)

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

    ! Update with explicit Runge-Kutta solver
    call explicit_rk%update(particles, old_time, delta_time, flow_field_fun)

    ! Evaluate flow field
    call flow_field_fun(time, mesh, flow_field)

    ! Write results
    call flow_field%write_to_csv("flow_field", ["vx", "vy", "vz"], timestep)
    call particles%write_to_csv("particles", ["x", "y", "z"], timestep)
  end do

end program main