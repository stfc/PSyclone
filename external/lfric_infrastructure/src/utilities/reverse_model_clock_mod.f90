!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> A timestepping clock implementation that runs in reverse.
!>
module reverse_model_clock_mod

  use clock_mod,     only : clock_type
  use constants_mod, only : i_timestep, r_second
  use log_mod,       only : log_event, log_level_error, log_scratch_space, &
                            log_set_timestep, log_forget_timestep

  implicit none

  private

  !> A clock which ticks in timesteps.
  !>
  !> The overall run (from step @f$n@f$ to step 1) may be split into a
  !> number of partial runs (subset of overall run). The clock ticks over a
  !> partial run from timestep @f$b@f$ to step @f$a@f$ where @f$a \geq 1@f$,
  !> @f$a < b@f$ and @f$b \leq n@f$.
  !>
  !>
  type, public, extends(clock_type) :: reverse_model_clock_type
    private
    integer(i_timestep) :: current_step
    integer(i_timestep) :: first_step
    integer(i_timestep) :: last_step
    real(r_second)      :: seconds_per_step
    logical             :: starting
  contains
    private
    procedure, public :: tick
    procedure, public :: get_first_step
    procedure, public :: get_step
    procedure, public :: get_last_step
    procedure, public :: get_seconds_per_step
    procedure, public :: seconds_from_steps
    procedure, public :: is_running
  end type reverse_model_clock_type

  interface reverse_model_clock_type
    procedure reverse_model_clock_constructor
  end interface reverse_model_clock_type

contains

  !> @brief Sets up the clock object before use.
  !>
  !> @param[in] first First date in the current run.
  !> @param[in] last Last date in the current run.
  !> @param[in] seconds_per_step Length of a timestep in seconds.
  !>
  function reverse_model_clock_constructor( first,            &
                                            last,             &
                                            seconds_per_step ) result(new_clock)

    implicit none

    integer(i_timestep),      intent(in) :: first
    integer(i_timestep),      intent(in) :: last
    real(r_second),           intent(in) :: seconds_per_step
    type(reverse_model_clock_type) :: new_clock

    if (first < 1) then
      write(log_scratch_space, '("First clock step must be positive")')
      call log_event(log_scratch_space, log_level_error)
    else
      new_clock%first_step = first
    end if

    if (last > new_clock%first_step) then
      write(log_scratch_space, '("Last clock step must be before first")')
      call log_event(log_scratch_space, log_level_error)
    else if (last < 1) then
      write(log_scratch_space, '("Last clock step must be positive")')
      call log_event(log_scratch_space, log_level_error)
    else
      new_clock%last_step = last
    end if

    if (seconds_per_step >= 0.0_r_second) then
      call log_event( 'Delta T must be less than zero.', log_level_error )
    else
      new_clock%seconds_per_step = seconds_per_step
    end if

    new_clock%current_step = new_clock%first_step
    new_clock%starting = .true.

  end function reverse_model_clock_constructor

  !> Gets the first step in the current run.
  !>
  !> @return Timestep, always greater than zero.
  !>
  function get_first_step( this )

    implicit none

    class(reverse_model_clock_type), intent(in) :: this
    integer(i_timestep) :: get_first_step

    get_first_step = this%first_step

  end function get_first_step


  !> Gets the last step in the current run.
  !>
  !> @return Timestep, may be the same as the first step.
  !>
  function get_last_step( this )

    implicit none

    class(reverse_model_clock_type), intent(in) :: this
    integer(i_timestep) :: get_last_step

    get_last_step = this%last_step

  end function get_last_step


  !> Gets the length of a timestep.
  !>
  !> @return Timestep length in seconds. Always less than zero.
  !>
  function get_seconds_per_step( this )

    implicit none

    class(reverse_model_clock_type), intent(in) :: this
    real(r_second) :: get_seconds_per_step

    get_seconds_per_step = this%seconds_per_step

  end function get_seconds_per_step


  !> Gets the current timestep.
  !>
  !> @return Timestep between first and last.
  !>
  function get_step( this )

    implicit none

    class(reverse_model_clock_type), intent(in) :: this
    integer(i_timestep) :: get_step

    get_step = max(this%current_step, this%last_step)

  end function get_step


  !> Indicates whether the clock is in the "running" state.
  !>
  !> The "running" state exists while the clock's current step is between its
  !> first and last step. i.e. it indicates the time period over which the
  !> model should run.
  !>
  !> @return True if clock is in running state.
  !>
  function is_running( this )

    implicit none

    class(reverse_model_clock_type), intent(in) :: this
    logical :: is_running

    is_running = (this%current_step >= this%last_step)

  end function is_running


  !> Converts a number of timesteps to a number of seconds.
  !>
  !> @return Seconds will be positive or negative depending on period.
  !>
  function seconds_from_steps( this, period )

    implicit none

    class(reverse_model_clock_type),   intent(in) :: this
    integer(i_timestep),       intent(in) :: period
    real(r_second) :: seconds_from_steps

    seconds_from_steps = period * this%seconds_per_step

  end function seconds_from_steps


  !> Advances the clock by one timestep.
  !>
  !> @return True if clock is still running.
  !>
  function tick( this )

    implicit none

    class(reverse_model_clock_type), intent(inout) :: this
    logical :: tick

    if (this%starting) then
      this%starting = .false.
    else
      this%current_step = this%current_step - 1
    end if

    if (this%is_running()) then
      call log_set_timestep( this%current_step )
    else
      call log_forget_timestep()
    end if

    tick = this%is_running()

  end function tick

end module reverse_model_clock_mod
