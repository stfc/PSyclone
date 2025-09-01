










!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> A module containing utility functions to contain events to be attached to
!> the ticking of a model clock
!>
module event_mod
  use constants_mod,   only : str_def, i_timestep
  use event_actor_mod, only : event_actor_type
  use clock_mod,       only : clock_type
  use linked_list_data_mod, only : linked_list_data_type
  implicit none

  private

  ! Type containing an event that will be attached to the model clock's tick
  type, extends(linked_list_data_type), public :: event_type
    private
    procedure(event_action), nopass, pointer :: action => null()
    class(event_actor_type),         pointer :: actor  => null()
    logical                                  :: active = .false.
  contains
    procedure, public :: is_active
    procedure, public :: happens
    procedure, public :: actor_name
    procedure, public :: set_active
    procedure, public :: get_actor_start
    procedure, public :: get_actor_stop
  end type event_type

  interface event_type
    procedure event_constructor
  end interface event_type

  ! Abstract interface for timestep event actions
  abstract interface
    subroutine event_action( actor, clock )
      import event_actor_type, clock_type
      class(event_actor_type), intent(inout) :: actor
      class(clock_type),       intent(in)    :: clock
    end subroutine event_action
  end interface


  public :: event_action

contains

  !> Constructs an event object
  !>
  !> @param[in] input_action The procedure that will be called during the event
  !> @param[in] input_actor  The object that will perform the action
  function event_constructor(input_action, input_actor) result(new_event)

    implicit none

    procedure(event_action), pointer, intent(in) :: input_action
    class(event_actor_type), pointer,  intent(in) :: input_actor
    type(event_type) :: new_event

    new_event%action => input_action
    new_event%actor => input_actor

  end function event_constructor

  !> Returns true if the event has been constructed and is ready to run
  function is_active(this) result(l_active)

    implicit none

    class(event_type), intent(inout) :: this
    logical                          :: l_active

    if (.not. associated(this%actor)) then
      l_active = .false.
      return
    end if

    if (this%actor%is_active()) then
      l_active = .true.
    else
      l_active = .false.
    end if

  end function is_active

  subroutine set_active(this, active)
    implicit none

    class(event_type), intent(inout) :: this
    logical, intent(in) :: active

    this%active = active

  end subroutine set_active

  !> Runs an instance of the event
  !>
  !> @param[in] clock The model clock
  subroutine happens(this, clock)

    implicit none

    class(event_type), intent(inout) :: this
    class(clock_type), intent(in)    :: clock

    call this%action( this%actor, clock )

  end subroutine happens

  function actor_name(this) result(name)
    implicit none

    class(event_type), intent(inout) :: this
    character(str_def) :: name

    name = this%actor%get_event_name()

  end function actor_name

  !> Delete the event
  !>
  subroutine delete_event(this)
    implicit none

    class(event_type), intent(inout) :: this

    this%active = .false.
    nullify(this%actor, this%action)

  end subroutine delete_event

  !> @brief Returns the start time of the actor attached to the event
  function get_actor_start(this) result(start_time)
    implicit none
    class(event_type), intent(in) :: this
    integer(i_timestep) :: start_time

    start_time = this%actor%get_start_time()

  end function get_actor_start

  !> @brief Returns the stop time of the actor attached to the event
  function get_actor_stop(this) result(stop_time)
    implicit none
    class(event_type), intent(in) :: this
    integer(i_timestep) :: stop_time

    stop_time = this%actor%get_stop_time()

  end function get_actor_stop

end module event_mod
