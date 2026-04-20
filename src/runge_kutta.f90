module RungeKuttaModule
  implicit none

  enum, bind(c)
    enumerator :: FORWARD_EULER
  end enum


  ! Explicit Runge-Kutta implementation
  type ExplicitRungeKuttaType
    ! The number of stages
    integer, private :: n_stages = 0
    ! Butcher tableau coefficients
    real, allocatable, private :: a(:, :), b(:), c(:)
    ! The time used for the intermediate evaluations
    real, private :: t_tmp
    ! The solution used for the intermediate evaluations
    real, allocatable, private :: x_tmp(:)
    ! Intermediate evaluations
    real, allocatable, private :: k(:, :)

  contains
    ! Initialization method
    procedure :: init => init_rk
    ! Allocate Butcher tableau coefficients and other data members
    procedure, private :: init_butcher => init_butcher_rk
    ! Update rule
    procedure :: update => update_rk
  end type ExplicitRungeKuttaType


  ! Forcing term function for Runge-Kutta schemes
  abstract interface
    subroutine f_template(t, x, f)
      ! The time
      real, intent(in) :: t
      ! The variable to be updated
      real, intent(in) :: x(:)
      ! The evaluation of the forcing term
      real, intent(out) :: f(:)
    end subroutine f_template
  end interface


  private :: init_rk, init_butcher_rk, update_rk

contains

  ! Initialization Runge-Kutta class based on the selected scheme
  subroutine init_rk(this, scheme, n)
    ! The RK class
    class(ExplicitRungeKuttaType), intent(out) :: this
    ! The selected RK scheme
    integer, intent(in) :: scheme
    ! The problem size, used to initialize the class members
    integer, intent(in) :: n

    ! Initialize Butcher tableau
    select case (scheme)
    case (FORWARD_EULER)
      call this%init_butcher(n_stages=1)
      this%b(1) = 1.0

    case default
      print *, "ERROR: Time-stepping method not implemented."
      call exit(1)
    end select

    ! Transpose a for better performance with column-major ordering
    this%a = transpose(this%a)

    ! Allocate variables used for the intermediate evaluations
    allocate(this%x_tmp(n))
    allocate(this%k(n, this%n_stages))
  end subroutine init_rk


  ! Allocate Butcher tableau coefficients
  subroutine init_butcher_rk(this, n_stages)
    ! The RK class
    class(ExplicitRungeKuttaType), intent(out) :: this
    ! The number of stages for the RK scheme
    integer, intent(in) :: n_stages

    this%n_stages = n_stages

    allocate(this%a(n_stages, n_stages))
    allocate(this%b(n_stages))
    allocate(this%c(n_stages))

    this%a = 0.0
    this%b = 0.0
    this%c = 0.0
  end subroutine init_butcher_rk


  ! Update rule for explicit RK schemes
  subroutine update_rk(this, x, t_old, delta_t, f_function)
    ! The RK class
    class(ExplicitRungeKuttaType), intent(inout) :: this
    ! The variable to be updated
    real, intent(inout) :: x(:)
    ! The previous time step
    real, intent(in) :: t_old
    ! The size of the time step
    real, intent(in) :: delta_t
    ! The forcing term function
    procedure(f_template) :: f_function
    ! Counters
    integer i, j

    ! First stage
    call f_function(t_old, x, this%k(:, 1))

    ! Other stages
    do i = 2, this%n_stages
      this%t_tmp = t_old + this%c(i) * delta_t
      this%x_tmp = x

      do j = 1, i - 1
        this%x_tmp = this%x_tmp + (delta_t * this%a(j, i)) * this%k(:, j)
      end do

      call f_function(this%t_tmp, this%x_tmp, this%k(:, i))
    end do

    ! Update the variable
    do i = 1, this%n_stages
      x = x + (delta_t * this%b(i)) * this%k(:, i)
    end do
  end subroutine update_rk

end module RungeKuttaModule