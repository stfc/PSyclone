!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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

  class(linked_list_data_type), intent (in)  :: self
  integer(i_def)                          :: id

  id = self%id

end function get_id

subroutine set_id(self, id)

  class(linked_list_data_type), intent (inout) :: self
  integer(i_def)  , intent(in)                 :: id

  self%id = id

end subroutine set_id



end module linked_list_data_mod
