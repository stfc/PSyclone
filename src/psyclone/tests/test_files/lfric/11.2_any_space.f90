! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program any_space_example

  ! Description: single kernel call in an invoke where the single operator
  ! argument is specified as any_space. 
  use operator_mod,             only : operator_type
  use testkern_any_space_3_mod, only : testkern_any_space_3_type

  implicit none

  type(operator_type) :: a

  call invoke(testkern_any_space_3_type(a))

end program any_space_example
