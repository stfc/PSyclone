! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program eval_invoke

  ! Test program containing a single invoke of a kernel that
  ! requires an evaluator on two, different function spaces.
  use field_mod,             only: field_type
  use testkern_eval_2fs_mod, only: testkern_eval_2fs_type

  implicit none

  type(field_type) :: f0, f1

  call invoke( testkern_eval_2fs_type(f0, f1) )

end program eval_invoke
