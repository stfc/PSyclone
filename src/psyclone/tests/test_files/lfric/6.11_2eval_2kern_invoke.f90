!-----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program eval_invoke

  ! Test program containing a single invoke of two kernels, each
  ! requiring an evaluator on W1 but with different target spaces.
  use field_mod,             only: field_type
  use testkern_eval_mod,     only: testkern_eval_type
  use testkern_eval_2fs_mod, only: testkern_eval_2fs_type

  implicit none

  type(field_type) :: f0, f1, g0, g1

  call invoke(  & ! Requires an evaluator (diff basis) on W1 evaluated at W0
               testkern_eval_type(f0, f1), &
               ! Requires an evaluator (diff basis) on W1 evaluated at W0 and W1
               testkern_eval_2fs_type(g0, g1))

end program eval_invoke
