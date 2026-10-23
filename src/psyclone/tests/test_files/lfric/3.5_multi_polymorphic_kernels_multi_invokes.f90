! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multi_functions_multi_invokes

  ! Description: multiple invoke calls, each involving the same
  ! polymorphic kernel.
  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use mixed_kernel_mod,    only: mixed_kernel_type
  use testkern_qr_mod,     only: testkern_qr_type

  implicit none

  type(field_type)             :: f1, f2, m1, m2
  type(field_type)             :: fieLd_r_def
  type(r_bl_field_type)        :: fiEld_r_bl
  type(quadrature_xyoz_type)   :: qr
  type(operator_type)          :: operator_r_def
  real(r_def)                  :: a
  integer(i_def)               :: istp
  real(r_def)                  :: Scalar_r_def
  real(r_bl)                   :: scalAr_r_bl

  call invoke(                                       &
       mixed_kernel_type(scalar_r_deF, field_R_def, opeRator_r_def), &
       testkern_qr_type(f1, f2, m1, a, m2, istp, qr) &
       )

  call invoke(                                        &
       mixed_kernel_type(scaLar_r_bl, fIeld_r_bl, opeRator_r_def), &
       testkern_qr_type(f1, f2, m1, a, m2, istp, qr)  &
       )

end program multi_functions_multi_invokes
