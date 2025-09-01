










!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Calendar implementation which understands time in terms of timesteps.
!>
module step_calendar_mod

  use calendar_mod,  only : calendar_type
  use constants_mod, only : i_timestep, str_def
  use log_mod,       only : log_event, log_level_error

  implicit none

  private

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Concrete calendar which understands timestep numbers.
  !>
  type, public, extends(calendar_type) :: step_calendar_type
    private
    character(str_def) :: origin
    character(str_def) :: start
  contains
    procedure :: get_origin
    procedure :: get_start
    procedure :: format_duration
    procedure :: format_instance
    procedure :: parse_duration
    procedure :: parse_instance
  end type step_calendar_type

  interface step_calendar_type
    module procedure step_calendar_constructor
  end interface step_calendar_type

contains

  !> Step calendar constructor
  !> Ensures that the calendar origin and start are initialised
  function step_calendar_constructor(calendar_origin, calendar_start) result(new_calendar)

    implicit none

    character(len=*), optional, intent(in) :: calendar_origin
    character(len=*), optional, intent(in) :: calendar_start
    type(step_calendar_type) :: new_calendar

    if (present(calendar_origin)) then
      new_calendar%origin = trim(calendar_origin)
    else
      new_calendar%origin = "origin of model run"
    end if

    if (present(calendar_start)) then
      new_calendar%start = trim(calendar_start)
    else
      new_calendar%start = "start of model run"
    end if

  end function step_calendar_constructor

  !> Get the calendar's origin.
  !>
  !> @param[in] this Object pointer.
  !> @return calendar_origin Outgoing calendar origin
  !>
  function get_origin( this ) result(calendar_origin)

    implicit none

    class(step_calendar_type), intent(in) :: this

    character(:), allocatable :: calendar_origin

    calendar_origin = this%origin

  end function get_origin

  !> Get the calendar's start.
  !>
  !> @param[in] this Object pointer.
  !> @return calendar_start Outgoing calendar start
  !>
  function get_start( this ) result(calendar_start)

    implicit none

    class(step_calendar_type), intent(in) :: this

    character(:), allocatable :: calendar_start

    calendar_start = this%start

  end function get_start

  !> Converts a number of timesteps to a string.
  !>
  !> @param[in] this Object pointer.
  !> @param[in] duration Number of timesteps.
  !> @return Number of timesteps in correctly sized string.
  !>
  function format_duration( this, duration ) result(string)

    implicit none

    class(step_calendar_type), intent(in) :: this
    integer(i_timestep),       intent(in) :: duration
    character(:), allocatable :: string

    integer :: string_size
    integer :: status

    ! Calculate how many characters are in a number, remembering to add one
    ! if a negative indicator will be required. This allows the string to be
    ! correctly sized.
    !
    if (duration > 0_i_timestep) then
      string_size = int(log10( real(duration))) + 1
    else if (duration < 0_i_timestep) then
      string_size = int(log10( real(-duration))) + 2
    else
        string_size = 1
    end if
    allocate( character(string_size) :: string, stat=status )
    if (status /= 0) then
      call log_event(                                                        &
            'step_calendar_type%format_duration: Unable to allocate string', &
             log_level_error )
    end if
    write( string, '(I0)') duration

  end function format_duration

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Converts a timestep number into a string.
  !>
  !> @param[in] this Object pointer.
  !> @param[in] instance Timestep number.
  !> @return Timestep number in correctly sized string.
  !>
  function format_instance( this, instance ) result(string)

    implicit none

    class(step_calendar_type), intent(in) :: this
    integer(i_timestep),       intent(in) :: instance
    character(:), allocatable :: string

    integer :: string_size
    integer :: status

    ! Calculate how many characters are in a number. This allows the string to
    ! be correctly sized.
    !
    if (instance > 0_i_timestep) then
      string_size = int(log10(real(instance))) + 1
    else if (instance == 0_i_timestep) then
      string_size = 1
    else
        call log_event(                                                      &
  'step_calendar_type%format_instance: No such thing as negative timesteps', &
                        log_level_error )
    end if
    allocate( character(string_size) :: string, stat=status )
    if (status /= 0) then
      call log_event(                                                        &
            'step_calendar_type%format_instance: Unable to allocate string', &
            log_level_error )
    end if
    write( string, '(I0)') instance

  end function format_instance

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Converts a string to a number of timesteps.
  !>
  !> @param[in] this Object pointer.
  !> @param[in] string Correctly formatted number of timesteps.
  !> @return Number of timesteps.
  !>
  function parse_duration( this, string ) result(duration)

    implicit none

    class(step_calendar_type), intent(in) :: this
    character(*),              intent(in) :: string
    integer(i_timestep) :: duration

    integer :: string_size
    integer :: format_size
    integer :: status
    character(:), allocatable :: fmt

    ! The "I0" formatting string does not work for input. Instead the exact
    ! number of digits to be read must be requested. Thus we need to construct
    ! a format string from the size of the string.
    !
    string_size = len(string)
    format_size = string_size + 3
    allocate( character(format_size) :: fmt, stat=status )
    if (status /= 0) then
        call log_event(                                                      &
             'step_calendar_type%parse_duration: Unable to allocate format', &
             log_level_error )
    end if
    write( fmt, '("(I", I0, ")")') string_size
    read( string, fmt) duration
    deallocate( fmt )

  end function parse_duration

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Converts a string to a timestep number.
  !>
  !> @param[in] this Object pointer.
  !> @param[in] string Correctly formatted timestep number.
  !> @return Timestep number.
  !>
  function parse_instance( this, string ) result(instance)

    implicit none

    class(step_calendar_type), intent(in) :: this
    character(*),              intent(in) :: string
    integer(i_timestep) :: instance

    integer :: string_size
    integer :: format_size
    integer :: status
    character(:), allocatable :: fmt

    ! The "I0" formatting string does not work for input. Instead the exact
    ! number of digits to be read must be requested. Thus we need to construct
    ! a format string from the size of the string.
    !
    string_size = len(string)
    format_size = string_size + 3
    allocate( character(format_size) :: fmt, stat=status )
    if (status /= 0) then
        call log_event(                                                      &
             'step_calendar_type%parse_instance: Unable to allocate format', &
             log_level_error )
    end if
    write( fmt, '("(I", I0, ")")') string_size
    read( string, fmt) instance
    deallocate( fmt )

    if (instance < 0) then
      call log_event(                                                        &
         'step_calendar_type%parse_instance: Instances may not be negative', &
         log_level_error )
    end if

  end function parse_instance

end module step_calendar_mod
