! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: multiple kernels requiring the same reference-element
  ! properties specified in an invoke call
  use constants_mod,         only: r_def
  use field_mod,             only: field_type
  use testkern_ref_elem_mod, only: testkern_ref_elem_type

  implicit none

  type(field_type) :: f1, f2, f3, f4, m1, m2, m3, m4
  real(r_def)      :: a

  call invoke( testkern_ref_elem_type(a, f1, f2, m1, m2), &
               testkern_ref_elem_type(a, f3, f4, m3, m4) )

end program single_invoke
