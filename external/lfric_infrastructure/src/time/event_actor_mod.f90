










!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> A module containing utility functions to contain events to be attached to
!> the ticking of a model clock
!>
module event_actor_mod

  use constants_mod,        only : str_def, i_timestep, IMDI
  use linked_list_data_mod, only : linked_list_data_type
  use log_mod,              only : log_event, log_scratch_space, LOG_LEVEL_ERROR

  implicit none

  private

  ! Abstract class used for timestep event interface
  type, extends(linked_list_data_type), public, abstract :: event_actor_type
    private
    character(str_def) :: event_name
    logical :: active = .false.
    logical :: constructed = .false.
    integer(i_timestep) :: start = IMDI
    integer(i_timestep) :: stop = IMDI
  contains
    procedure, public :: init_event_actor
    procedure, public :: get_event_name
    procedure, public :: get_start_time
    procedure, public :: get_stop_time
    procedure, public :: is_active
    procedure, public :: set_active
  end type event_actor_type

contains

  !> @brief Initialiser for the event actor
  !> @param[in] name  The name of the event actor
  !> @param[in] start The start timestep of the event
  !> @param[in] stop  The time to stop the event
  subroutine init_event_actor(this, name, start, stop)
    implicit none
    class(event_actor_type), intent(inout) :: this
    character(*), intent(in) :: name
    integer(i_timestep), optional, intent(in) :: start
    integer(i_timestep), optional, intent(in) :: stop

    if(this%constructed) then
      write(log_scratch_space, '(A)') trim(name) // &
                                      " event actor type already initialised"
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end if

    if(present(start)) then
      this%start = start
    else
      this%start = 0_i_timestep
      this%active = .true.
    end if

    if(present(stop)) then
      this%stop = stop
    end if

    this%event_name = name // "_event"
    this%constructed = .true.
  end subroutine init_event_actor

  !> Returns the name of the event
  function get_event_name(this) result(name)

    implicit none

    class(event_actor_type), intent(in) :: this
    character(str_def) :: name

    name = this%event_name

  end function get_event_name

  !> @brief Getter for the start time
  !> @return start_time The start time of the event
  function get_start_time(this) result(start_time)
    implicit none
    class(event_actor_type), intent(in) :: this
    integer(i_timestep) :: start_time
    start_time = this%start
  end function get_start_time

  !> @brief Getter for the stop time
  !> @return stop_time The stop time of the event
  function get_stop_time(this) result(stop_time)
    implicit none
    class(event_actor_type), intent(in) :: this
    integer(i_timestep) :: stop_time

    stop_time = this%stop
  end function get_stop_time

  !> Returns true if the event actor can be used
  function is_active(this) result(l_active)

    implicit none

    class(event_actor_type), intent(inout) :: this
    logical                                :: l_active

    l_active = this%active

  end function is_active

  !> @brief Set the active state of the event
  !> @param[in] state The state for the event to be set to.
  subroutine set_active(this, state)
    implicit none
    class(event_actor_type), intent(inout) :: this
    logical, intent(in) :: state

    this%active = state

  end subroutine set_active

end module event_actor_mod
