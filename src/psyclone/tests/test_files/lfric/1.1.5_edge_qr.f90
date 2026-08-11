! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single kernel requiring edge quadrature specified in an
  ! invoke call
  use constants_mod,         only: r_def, i_def
  use field_mod,             only: field_type
  use quadrature_edge_mod,   only: quadrature_edge_type
  use testkern_qr_edges_mod, only: testkern_qr_edges_type

  implicit none

  type(field_type)           :: f1, f2, m1, m2
  type(quadrature_edge_type) :: qr
  real(r_def)                :: a
  integer(i_def)             :: istp

  call invoke( testkern_qr_edges_type(f1, f2, m1, a, m2, istp, qr) )

end program single_invoke
