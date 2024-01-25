!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

!> @brief Linked list data integer type

!> @details A type to hold ints for putting in a linked list
!>         inherits from linked_list_data_type

module linked_list_int_mod

  use linked_list_data_mod, only : linked_list_data_type

  implicit none

  private

  type, extends(linked_list_data_type), public        :: linked_list_int_type
    private
    contains
    ! Nothing in here - it's all in the base class
    procedure, public :: clear

    final :: linked_list_int_destructor

  end type linked_list_int_type

  interface linked_list_int_type
       module procedure int_constructor
  end interface linked_list_int_type

contains

! constructor
type(linked_list_int_type) function int_constructor(id)

  implicit none

  integer, intent(in)           :: id
  call int_constructor%set_id(id)

end function int_constructor

subroutine clear( self )
  implicit none
  class(linked_list_int_type), intent(inout) :: self
end subroutine clear

subroutine linked_list_int_destructor(self)
  implicit none
  type(linked_list_int_type), intent(inout) :: self
  call self%clear()
end subroutine linked_list_int_destructor

end module linked_list_int_mod
