! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program any_space_example

  ! Description: single kernel call in an invoke where the field and operator
  ! arguments are specified on any_space with basis and differential basis
  ! functions on any_space for quadrature rule.
  use field_mod,                only : field_type
  use operator_mod,             only : operator_type
  use quadrature_xyoz_mod,      only : quadrature_xyoz_type
  use testkern_any_space_4_mod, only : testkern_any_space_4_type

  implicit none

  type(field_type)           :: a, a_field_with_a_very_long_name
  type(operator_type)        :: b, c, an_operator_with_a_very_long_name, e
  type(quadrature_xyoz_type) :: qr

  call invoke(testkern_any_space_4_type(a, b, c, an_operator_with_a_very_long_name, e, a_field_with_a_very_long_name, qr))

end program any_space_example
