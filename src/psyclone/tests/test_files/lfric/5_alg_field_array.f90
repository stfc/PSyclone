! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_function

  ! Description: field_type arrays indexed in the invoke
  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use testkern_qr_mod,     only: testkern_qr_type

  implicit none

  type(field_type)           :: f0(2), f1(2,2)
  type(quadrature_xyoz_type) :: qr
  real(r_def)                :: b(2), a(8)
  integer(i_def)             :: iflag(4)
  integer(i_def)             :: index, index1, index2, index3

  call invoke(                                                   &
       testkern_qr_type(f0(1), f1(1,1), f1(2,index), b(1),       &
                        f1(index,index2(index3)), iflag(2), qr), &
       testkern_qr_type(f1(index,index2(index3)),                &
                        f1(2,index), f1(1,1), a(index1), f0(1),  &
                        iflag(index2(index3)), qr)               &
          )

end program single_function
