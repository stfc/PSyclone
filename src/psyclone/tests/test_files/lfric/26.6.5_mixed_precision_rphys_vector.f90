! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Example where the field is dereferenced from an 'r_um_field_vector_type'
! and therefore has no type information. The LFRic rules state that an
! 'r_um_field_vector_type' can only contain fields of type
! 'r_um_field_type' in LFRic code. This is checked at runtime in the
! LFRic model by using a 'select' statement in the algorithm code.

module vector_type

  use constants_mod,           only : r_um
  use vector_mod,              only : abstract_vector_type
  use r_um_field_vector_mod, only : r_um_field_vector_type
  use r_um_field_mod,        only : r_um_field_type
  use testkern_mod,            only : testkern_type

  type :: some_type
     type(r_um_field_vector_type) :: vec_type(10)
   contains
     procedure, public :: my_sub
  end type some_type

  contains

  subroutine my_sub(self, x, m1, m2)
    class(some_type), intent(inout) :: self
    type(r_um_field_vector_type), intent(inout) :: x
    type(r_um_field_type), intent(inout) :: m1, m2
    real(r_um) :: a
    call invoke(testkern_type(a, x%vector(1), self%vec_type(1)%vector(1), m1, m2))
  end subroutine my_sub

end module vector_type
