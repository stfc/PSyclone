! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_6

  ! Two calls to the same kernel within an invoke where the arguments are
  ! specified as any_space

  use constants_mod,            only : i_def
  use field_mod,                only : field_type
  use operator_mod,             only : operator_type
  use testkern_any_space_2_mod, only : testkern_any_space_2_type

  implicit none

  type(field_type)    :: f1, f2
  type(operator_type) :: op
  integer(i_def)      :: scalar

  call invoke(                                        &
       testkern_any_space_2_type(f1, f2, op, scalar), &
       testkern_any_space_2_type(f2, f1, op, scalar)  &
       )

end program multikernel_invokes_6
