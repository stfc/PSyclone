! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Example where the field is dereferenced from an abstract_vector_type
! and therefore its precision can't be determined by looking at the
! declaration. As a result PSyclone is currently not able to determine
! the type and will raise an exception.

module vector_type

  use constants_mod,    only : r_def
  use vector_mod,       only : abstract_vector_type
  use field_vector_mod, only : field_vector_type
  use field_mod,        only : field_type
  use testkern_mod,     only : testkern_type

  type :: some_type
     type(field_vector_type) :: vec_type(10)
   contains
     procedure, public :: my_sub
  end type some_type

  contains

  subroutine my_sub(self, x, m1, m2)
    class(some_type), intent(inout) :: self
    class (abstract_vector_type), intent(inout) :: x
    type(field_type), intent(inout) :: m1, m2
    real(r_def) :: a
    select type (x)
    type is (field_vector_type)
      call invoke(testkern_type(a, x%vector(1), self%vec_type(1)%vector(1), m1, m2))
    class default
      print *,"Error"
    end select
  end subroutine my_sub

end module vector_type
