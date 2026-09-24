! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: one reduction builtin followed by an access to 
  ! the reduction value.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_anys_vector_write_mod, only: testkern_anys_vector_write_type
  implicit none

  type(field_type)           :: f1(2), f2, f3, f4(2)
  type(quadrature_xyoz_type) :: qr

  call invoke( testkern_anys_vector_write_type(f1, f2, f3, qr), &
               testkern_anys_vector_write_type(f4, f2, f3, qr) &
           )

end program single_invoke
