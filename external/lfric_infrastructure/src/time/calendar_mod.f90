










!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Defines a generic calendar.
!>
module calendar_mod

  use constants_mod, only : i_timestep

  implicit none

  private

  !> Parent of all calendar implementations.
  !>
  type, public, abstract :: calendar_type
    private
  contains
    private
    procedure(get_origin_if),      public, deferred :: get_origin
    procedure(get_start_if),       public, deferred :: get_start
    procedure(format_duration_if), public, deferred :: format_duration
    procedure(format_instance_if), public, deferred :: format_instance
    procedure(parse_duration_if),  public, deferred :: parse_duration
    procedure(parse_instance_if),  public, deferred :: parse_instance
  end type calendar_type

  abstract interface

    !> Get the calendar's origin.
    !>
    !> @param[in] this Object pointer.
    !> @return calendar_origin Outgoing calendar origin
    !>
    function get_origin_if( this ) result(calendar_origin)
      import calendar_type
      implicit none
      class(calendar_type), intent(in) :: this
      character(:), allocatable :: calendar_origin
    end function get_origin_if

    !> Get the calendar's start.
    !>
    !> @param[in] this Object pointer.
    !> @return calendar_start Outgoing calendar start
    !>
    function get_start_if( this ) result(calendar_start)
      import calendar_type
      implicit none
      class(calendar_type), intent(in) :: this
      character(:), allocatable :: calendar_start
    end function get_start_if

    !> Produces a human readable string from a calendar specific duration.
    !>
    !> @param[in] duration Period of time in calendar terms.
    !>
    function format_duration_if( this, duration ) result(string)
      import calendar_type, i_timestep
      implicit none
      class(calendar_type), intent(in) :: this
      integer(i_timestep),  intent(in) :: duration
      character(:), allocatable :: string
    end function format_duration_if

    !> Produces a human readable string from a calendar specific instance.
    !>
    !> @param[in] instance Instant in time in calendar terms.
    !>
    function format_instance_if( this, instance ) result(string)
      import calendar_type, i_timestep
      implicit none
      class(calendar_type), intent(in) :: this
      integer(i_timestep),  intent(in) :: instance
      character(:), allocatable :: string
    end function format_instance_if

    !> Converts a human readable string into a duration object.
    !>
    !> @param[in] string Human readable string in calendar form.
    !>
    function parse_duration_if( this, string ) result(duration)
      import calendar_type, i_timestep
      implicit none
      class(calendar_type), intent(in) :: this
      character(*),         intent(in) :: string
      integer(i_timestep) :: duration
    end function parse_duration_if

    !> Converts a human readable string into a time instance object.
    !>
    !> @param[in] string Human readable string in calendar form.
    !>
    function parse_instance_if( this, string ) result(instance)
      import calendar_type, i_timestep
      implicit none
      class(calendar_type), intent(in) :: this
      character(*),         intent(in) :: string
      integer(i_timestep) :: instance
    end function parse_instance_if

  end interface

end module calendar_mod
