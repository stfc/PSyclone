!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!> Module for function_space_pointer_type which is a container for a pointer to
!> a function_space_type.
!>
!> function_space_pointer_type is required as a work through, as the linked list
!> class can only hold objects which are children of the linked_list_data_type.
!>

module function_space_pointer_mod

  use constants_mod,        only : i_def
  use linked_list_data_mod, only : linked_list_data_type
  use function_space_mod,   only : function_space_type

  implicit none

  private

  !===============================================================================
  type, extends(linked_list_data_type), public :: function_space_pointer_type

    private
    type(function_space_type), pointer :: function_space_target => null()

  contains
    !> @brief Returns a pointer to the function space that this object contains.
    !> @return function_space_target Pointer to the function space object.
    procedure, public :: get_target

    procedure, public :: clear

    final :: function_space_pointer_destructor

  end type function_space_pointer_type

  interface function_space_pointer_type
    module procedure function_space_pointer_constructor
  end interface


contains ! Module procedures

  !===============================================================================
  !> Creates a linked list item which refers to a function space.
  !> @return instance function_space_pointer_type
  !>
  function function_space_pointer_constructor(function_space) result(instance)

    implicit none

    type(function_space_pointer_type) :: instance
    type(function_space_type), intent(in), pointer :: function_space

    call instance%set_id(function_space%get_id())
    instance%function_space_target => function_space

  end function function_space_pointer_constructor


  function get_target(self) result(function_space_target)

    implicit none

    class(function_space_pointer_type), intent(in), target :: self
    type(function_space_type), pointer :: function_space_target

    function_space_target => self%function_space_target

  end function get_target

  subroutine clear(self)

    implicit none

    class(function_space_pointer_type), intent(inout) :: self

    nullify(self%function_space_target)

  end subroutine clear

  subroutine function_space_pointer_destructor(self)

    implicit none

    type(function_space_pointer_type), intent(inout) :: self

    call self%clear()

  end subroutine function_space_pointer_destructor

  !===============================================================================
end module function_space_pointer_mod
