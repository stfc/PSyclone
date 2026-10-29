! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program eval_invoke

  ! Test program containing a single invoke of two kernels, each requiring
  ! an evaluator and writing to a field declared to be on the same space

  use field_mod,                    only: field_type
  use testkern_eval_anydspace1_mod, only: testkern_eval_anydspace1_type
  use testkern_eval_anydspace2_mod, only: testkern_eval_anydspace2_type

  implicit none

  type(field_type) :: f0, f1, f2, f3

  call invoke(                                    &
       testkern_eval_anydspace1_type(f0, f1, f2), &
       testkern_eval_anydspace2_type(f0, f1, f3)  &
       )

end program eval_invoke
