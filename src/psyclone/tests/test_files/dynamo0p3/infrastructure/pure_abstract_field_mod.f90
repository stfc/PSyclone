!-----------------------------------------------------------------------------
! (C) Crown copyright 2019-2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
!-----------------------------------------------------------------------------
!
!> @brief A module containing the abstract type that is the parent to all
!>        child field types.
!>
!> @details All field types (and pointer field types) are part of the same
!>          family. This module contains the pure abstract type that will be
!>          the parent to all of them

module pure_abstract_field_mod
  use linked_list_data_mod,        only: linked_list_data_type
  implicit none

  private

  !> Pure abstract field type that is the parent of any field type or field
  !> pointer type in the field object hierarchy
  type, extends(linked_list_data_type), public, abstract :: &
                                                 pure_abstract_field_type
  contains
  end type pure_abstract_field_type

contains

end module pure_abstract_field_mod
