!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief An abstract class from which all external fields inherit.
!>
!> @details The breaking of field data encapsultion is controlled by only
!>          allowing it in the PSy layer and through use of external fields.
!>          This is the abstract class from which all implementations of
!>          external fields inherit
!
module abstract_external_field_mod
  use constants_mod,        only: i_def
  use field_mod,            only: field_type
  use linked_list_data_mod, only: linked_list_data_type
  implicit none
  private

type, extends(linked_list_data_type), public, abstract :: &
                                               abstract_external_field_type
  private
  !> The lfric field that the external data will be associated with
  type(field_type), pointer      :: lfric_field
contains
  !> Initialises data held in the abstract
  procedure, public :: abstract_external_field_initialiser
  !> Copy data from the LFRic field
  procedure(copy_from_lfric_interface), deferred :: copy_from_lfric
  !> Copy data to the LFRic field
  procedure(copy_to_lfric_interface),   deferred :: copy_to_lfric
  !> Gets the lfric field pointer
  procedure, public :: get_lfric_field_ptr
  !> Destroys the abstract external field contents
  procedure, public :: abstract_external_field_destructor
end type abstract_external_field_type

abstract interface
  subroutine copy_from_lfric_interface( self, return_code )
    import abstract_external_field_type
    import :: i_def
    implicit none
    class( abstract_external_field_type ), intent(inout) :: self
    integer(i_def), intent(out), optional :: return_code
  end subroutine copy_from_lfric_interface
  subroutine copy_to_lfric_interface( self, return_code )
    import abstract_external_field_type
    import :: i_def
    implicit none
    class( abstract_external_field_type ), intent(inout) :: self
    integer(i_def), intent(out), optional :: return_code
  end subroutine copy_to_lfric_interface
end interface

contains

!> Initialises lfric field pointer held in the abstract
!> @param [in] pointer to an lfric field
subroutine abstract_external_field_initialiser( self, &
                                                lfric_field_ptr )
  implicit none

  class(abstract_external_field_type), intent(inout) :: self
  type(field_type), pointer, intent(in)              :: lfric_field_ptr

  self%lfric_field => lfric_field_ptr

end subroutine abstract_external_field_initialiser

!> Gets the lfric field pointer
!> @return A pointer to the lfric field
function get_lfric_field_ptr( self ) result( lfric_field_ptr )

  implicit none

  class(abstract_external_field_type), intent(in) :: self
  type(field_type), pointer                       :: lfric_field_ptr

  lfric_field_ptr => self%lfric_field

end function get_lfric_field_ptr

!> Destroys the abstract external field contents
subroutine abstract_external_field_destructor( self )
  implicit none

  class(abstract_external_field_type), intent(inout) :: self

  self%lfric_field => null()

end subroutine abstract_external_field_destructor

end module abstract_external_field_mod
