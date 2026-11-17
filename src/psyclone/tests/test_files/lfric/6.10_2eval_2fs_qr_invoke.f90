!-----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program eval_invoke

  ! Test program containing a single invoke of two kernels that
  ! require evaluators and one that requires quadrature
  use constants_mod,         only: r_def, i_def
  use field_mod,             only: field_type
  use operator_mod,          only: operator_type
  use quadrature_xyoz_mod,   only: quadrature_xyoz_type
  use testkern_eval_2fs_mod, only: testkern_eval_2fs_type
  use testkern_eval_op_mod,  only: testkern_eval_op_type
  use testkern_qr_mod,       only: testkern_qr_type

  implicit none

  type(field_type)           :: f0, f1, f2, m1, m2
  type(operator_type)        :: op1
  type(quadrature_xyoz_type) :: qr
  real(r_def)                :: a
  integer(i_def)             :: istp

  call invoke(                          &
       ! Requires diff basis on W1, evaluated at W0 and W1
       testkern_eval_2fs_type(f0, f1),  &
       ! Requires basis on W2 and diff-basis on W3, both evaluated
       ! on W0 (the to-space of the operator that is written to)
       testkern_eval_op_type(op1, m2),  &
       ! Requires XYoZ quadrature: basis on W1, diff basis on W2 and
       ! basis+diff basis on W3.
       testkern_qr_type(f1, f2, m1, a, m2, istp, qr))

end program eval_invoke
