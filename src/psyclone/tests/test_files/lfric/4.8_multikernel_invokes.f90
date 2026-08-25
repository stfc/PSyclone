! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_7

  ! Multiple kernel calls within an invoke where the fields updated by
  ! the two kernels are on different spaces

  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use ru_kernel_mod,       only: ru_kernel_type
  use testkern_mod,        only: testkern_type

  implicit none

  type(field_type)           :: a, b, c, d, e(3), f, g, h
  real(r_def)                :: ascalar, rdt
  integer(i_def)             :: istp
  type(quadrature_xyoz_type) :: qr

  call invoke(                                            &
               ! h is written, rest are read-only
               testkern_type(rdt, h, f, c, d),            &
               ! b is written, rest are read-only
               testkern_type(rdt, b, f, c, d),            &
               ! b is gh_inc, rest are read-only
               ru_kernel_type(b, a, istp, rdt, c, e, qr), &
               ! g is gh_inc, rest are read-only
               ru_kernel_type(g, a, istp, rdt, c, e, qr), &
               ! f is written, rest are read-only
               testkern_type(ascalar, f, b, c, d) )

  ! => b and h must be intent(inout)
  ! => g and f must be intent(inout)
  ! => a, c, d and e are intent(in)
end program multikernel_invokes_7
