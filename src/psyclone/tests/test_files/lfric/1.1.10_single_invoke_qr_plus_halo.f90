! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: a single kernel that requires XYoZ quadrature and that operates
  ! on halo cells specified in an invoke call.
  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use testkern_qr_and_halo_only_mod, only: testkern_qr_and_halo_only_type

  implicit none

  type(field_type)           :: f1, f2, m1, m2
  type(quadrature_xyoz_type) :: qr
  real(r_def)                :: a
  integer(i_def)             :: istp, hdepth

  call invoke(                                       &
       testkern_qr_and_halo_only_type(f1, f2, m1, a, m2, istp, qr, hdepth) &
          )

end program single_invoke
