! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program any_space_example

  ! Description: single kernel call in an invoke where the field arguments are
  ! specified on any_space with basis functions on any_space for quadrature rule.
  use constants_mod,            only : r_def
  use field_mod,                only : field_type
  use quadrature_xyoz_mod,      only : quadrature_xyoz_type
  use testkern_any_space_1_mod, only : testkern_any_space_1_type

  implicit none

  type(field_type)           :: a, b, c(3)
  type(quadrature_xyoz_type) :: qr
  real(r_def)                :: rdt

  call invoke(testkern_any_space_1_type(a, rdt, b, c, qr))

end program any_space_example
