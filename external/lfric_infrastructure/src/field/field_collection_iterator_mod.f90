










!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief Provides functionality for iterating over all members of a field
!>        collection
!>
!> @details Provides functionality for iteratively returning every member
!>          of a field collection. The order of the fields returned is
!>          not defined and can change if the implementation of the field
!>          collection is changed
!
module field_collection_iterator_mod

  use constants_mod,           only: i_def, l_def
  use field_collection_mod,    only: field_collection_type
  use field_mod,               only: field_type, &
                                     field_pointer_type
  use field_real32_mod,        only: field_real32_type, &
                                     field_real32_pointer_type
  use field_real64_mod,        only: field_real64_type, &
                                     field_real64_pointer_type
  use field_parent_mod,        only: field_parent_type
  use integer_field_mod,       only: integer_field_type, &
                                     integer_field_pointer_type
  use log_mod,                 only: log_event, log_scratch_space, &
                                     LOG_LEVEL_ERROR
  use linked_list_data_mod,    only: linked_list_data_type
  use linked_list_mod,         only: linked_list_type, &
                                     linked_list_item_type

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! Type that iterates through a field collection
  !-----------------------------------------------------------------------------
  type, public :: field_collection_iterator_type
    private
    !> A pointer to the field collection being iterated over
    type(field_collection_type), pointer :: collection
    !> A pointer to the linked list item within the collection that will
    !> contain the next field to be returned
    type(linked_list_item_type), pointer :: current
  contains
    procedure, public :: initialise
    procedure, public :: next
    procedure, public :: has_next
  end type field_collection_iterator_type

contains

!> Initialise a field collection iterator
!> @param [in] collection The collection to iterate over
subroutine initialise(self, collection)

  implicit none

  class(field_collection_iterator_type) :: self
  type(field_collection_type), target :: collection

  ! Store a pointer to the collection being iterated over
  self%collection => collection

  ! Start the iterator at the beginning of the field list.
  nullify(self%current)
  self%current => self%collection%get_next_item(self%current)

end subroutine initialise

!> Returns the next field from the collection
!> @return A polymorphic field pointer to either a field or field pointer
!>         that is next in the collection
function next(self) result (field)

  implicit none

  class(field_collection_iterator_type), intent(inout), target :: self
  class(field_parent_type), pointer :: field

  ! Empty lists are valid
  !
  if (.not. associated(self%current)) then
    field => null()
    return
  end if

  ! Extract a pointer to the current field in the collection
  select type(listfield => self%current%payload)
    type is (field_real32_type)
      field => listfield
    type is (field_real64_type)
      field => listfield
    type is (integer_field_type)
      field => listfield
    type is (field_real32_pointer_type)
      field => listfield%field_ptr
    type is (field_real64_pointer_type)
      field => listfield%field_ptr
    type is (integer_field_pointer_type)
      field => listfield%field_ptr
  end select
  ! Move the current item pointer onto the next field in the collection
  self%current => self%collection%get_next_item(self%current)

end function next

!> Checks if there are any further fields in the collection being iterated over
!> @return next true if there is another field in the collection, and false if
!> there isn't.
function has_next(self) result(next)
  implicit none
  class(field_collection_iterator_type), intent(in) :: self
  logical(l_def) :: next
  next = .true.
  if(.not.associated(self%current)) next = .false.
end function has_next

end module field_collection_iterator_mod
