! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module vector_type

  use constants_mod,         only : r_bl
  use vector_mod,            only : abstract_vector_type
  use r_bl_field_vector_mod, only : r_bl_field_vector_type
  use r_bl_field_mod,        only : r_bl_field_type
  use testkern_mod,          only : testkern_type

  type :: some_type
     type(r_bl_field_vector_type) :: vec_type(10)
   contains
     procedure, public :: my_sub
  end type some_type

  contains

  subroutine my_sub(self, x, m1, m2)
    class(some_type), intent(inout) :: self
    type(r_bl_field_vector_type), intent(inout) :: x
    type(r_bl_field_type), intent(inout) :: m1, m2
    real(r_bl) :: a
    call invoke(testkern_type(a, x%vector(1), self%vec_type(1)%vector(1), m1, m2))
  end subroutine my_sub

end module vector_type
