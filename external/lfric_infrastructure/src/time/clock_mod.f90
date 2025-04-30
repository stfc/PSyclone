!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Abstract timestepping clock
!>
module clock_mod

  use constants_mod, only : i_timestep, r_second

  implicit none

  private

  type, public, abstract :: clock_type
    private
  contains
    private
    procedure(get_first_if),          public, deferred :: get_first_step
    procedure(get_step_if),           public, deferred :: get_step
    procedure(get_last_if),           public, deferred :: get_last_step
    procedure(tick_if),               public, deferred :: tick
    procedure(seconds_per_step_if),   public, deferred :: get_seconds_per_step
  end type clock_type

  abstract interface
    function get_first_if( this )
      import clock_type, i_timestep
      implicit none
      class(clock_type), intent(in) :: this
      integer(i_timestep) :: get_first_if
    end function get_first_if

    function get_step_if( this )
      import clock_type, i_timestep
      implicit none
      class(clock_type), intent(in) :: this
      integer(i_timestep) :: get_step_if
    end function get_step_if

    function get_last_if( this )
      import clock_type, i_timestep
      implicit none
      class(clock_type), intent(in) :: this
      integer(i_timestep) :: get_last_if
    end function get_last_if

    function tick_if( this )
      import clock_type
      implicit none
      class(clock_type), intent(inout) :: this
      logical :: tick_if
    end function tick_if

    function seconds_per_step_if( this )
      import clock_type, r_second
      implicit none
      class(clock_type), intent(in) :: this
      real(r_second) :: seconds_per_step_if
    end function seconds_per_step_if

  end interface

end module clock_mod
