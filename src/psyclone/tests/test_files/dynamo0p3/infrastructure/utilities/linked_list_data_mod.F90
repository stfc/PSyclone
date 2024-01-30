!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

!> @brief Linked list data type

!> @details A generic linked list data type - anything that needs to
!>          be put in a linked list must inherit from this

module linked_list_data_mod

  use constants_mod,        only    : i_def

  implicit none

  type, abstract, public :: linked_list_data_type
    private
    integer(i_def) :: id
  contains
    procedure, public :: get_id
    procedure, public :: set_id
  end type linked_list_data_type

contains

function get_id(self) result(id)

  implicit none

  class(linked_list_data_type), intent (in)  :: self
  integer(i_def)                             :: id

  id = self%id

end function get_id

subroutine set_id(self, id)

  implicit none

  class(linked_list_data_type), intent (inout) :: self
  integer(i_def)  , intent(in)                 :: id

  self%id = id

end subroutine set_id

end module linked_list_data_mod
