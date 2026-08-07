! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program any_space_example

  ! Description: single kernel call in an invoke where the field and operator
  ! arguments are specified on any_space.
  use constants_mod,            only : i_def
  use field_mod,                only : field_type
  use operator_mod,             only : operator_type
  use testkern_any_space_2_mod, only : testkern_any_space_2_type

  implicit none

  type(field_type)    :: a, b
  type(operator_type) :: c
  integer(i_def)      :: istp

  call invoke(testkern_any_space_2_type(a, b, c, istp))

end program any_space_example
