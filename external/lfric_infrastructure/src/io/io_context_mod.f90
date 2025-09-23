










!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Specifies the interface for all I/O context classes.
!>
module io_context_mod

  use constants_mod,   only : str_def, i_timestep
  use clock_mod,       only : clock_type
  use event_actor_mod, only : event_actor_type
  use linked_list_mod, only : linked_list_type

  implicit none

  private

  !> @brief All context classes inherit this interface.
  !>
  type, public, abstract, extends(event_actor_type) :: io_context_type
    private
    character(str_def) :: context_name
  contains
    private
    procedure, public :: initialise_io_context
    procedure, public :: get_context_name
    procedure(get_filelist_if), public, deferred :: get_filelist
    procedure(set_current_if) , public, deferred :: set_current
  end type io_context_type

  abstract interface
    !> Gets the list of files associated with this context.
    !>
    !> @return Linked list of file objects.
    !>
    function get_filelist_if( this ) result(filelist)
      import linked_list_type, io_context_type
      implicit none
      class(io_context_type), intent(in), target :: this
      type(linked_list_type), pointer :: filelist
    end function get_filelist_if
  end interface

  abstract interface
    !> Sets the context as current
    !>
    subroutine set_current_if( this )
      import io_context_type
      implicit none
      class(io_context_type), intent(inout) :: this
    end subroutine set_current_if
  end interface

  abstract interface
    !> @brief Callback interface for bespoke IO configuration
    !> @param[in] clock     Clock to be passed in at call site
    subroutine callback_clock_arg(clock)
      use clock_mod, only : clock_type
      implicit none
      class(clock_type), intent(in) :: clock
    end subroutine callback_clock_arg
  end interface

  public :: callback_clock_arg

contains

  !> @brief Initialise the abstract I/O context
  !> @param name The name of the I/O context
  subroutine initialise_io_context(this, name, start, stop)
    implicit none
    class(io_context_type), intent(inout) :: this
    character(*), intent(in) :: name
    integer(i_timestep), optional, intent(in) :: start
    integer(i_timestep), optional, intent(in) :: stop

    this%context_name = trim(name)
    call this%init_event_actor(name, start, stop)

  end subroutine initialise_io_context

  !> @brief Returns the name of this I/O context
  !> @return name The name of the I/O context
  function get_context_name(this) result(name)
    implicit none
    class(io_context_type) :: this
    character(str_def) :: name

    name = this%context_name

  end function get_context_name

end module io_context_mod
