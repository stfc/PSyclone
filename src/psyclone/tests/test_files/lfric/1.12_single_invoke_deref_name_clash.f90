! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single function specified in an invoke call with one
  ! argument obtained by dereferencing a derived type. The 2nd and 3rd
  ! arguments should have different names in the PSy layer.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_mod,  only: testkern_type

  implicit none

  type container_type
     type(field_type) :: my_field
  end type container_type
  type(container_type) :: f1
  type(field_type) :: m1, m2, f1_my_field
  real(r_def)      :: a

  call invoke(                                            &
       testkern_type(a, f1_my_field, f1%my_field, m1, m2) &
          )

end program single_invoke
