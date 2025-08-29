!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief An empty io context.
!>
module empty_io_context_mod

  use io_context_mod,  only : io_context_type
  use linked_list_mod, only : linked_list_type

  implicit none

  private

  !> @brief Contains an instance of an empty context.
  !>
  type, public, extends(io_context_type) :: empty_io_context_type
    private
  contains
    private
    procedure, public :: get_filelist
    procedure, public :: set_current
    procedure, public :: initialise => empty_io_context_initialiser
  end type empty_io_context_type

contains

  subroutine empty_io_context_initialiser(this, name)
    implicit none
    class(empty_io_context_type), intent(inout) :: this
    character(*), intent(in) :: name

    call this%initialise_io_context(name)

  end subroutine empty_io_context_initialiser

  !> @brief  Deferred procedure implementation - does nothing
  !>
  !> @return Null linked list pointer
  function get_filelist( this ) result(filelist)

    implicit none

    class(empty_io_context_type), intent(in), target :: this
    type(linked_list_type), pointer :: filelist

    filelist => null()

  end function get_filelist

  !> @brief  Deferred procedure implementation - does nothing
  subroutine set_current( this )

    implicit none

    class(empty_io_context_type), intent(inout) :: this

  end subroutine set_current

end module empty_io_context_mod
