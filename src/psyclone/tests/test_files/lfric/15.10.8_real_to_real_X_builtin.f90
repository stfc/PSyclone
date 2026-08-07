! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (conversion of real-valued to
  ! real-valued field elements) specified in an invoke call.
  use r_solver_field_mod, only: r_solver_field_type
  use r_tran_field_mod,   only: r_tran_field_type
  use field_mod,          only: field_type

  implicit none

  type(field_type)          :: f1
  type(r_tran_field_type)   :: f2
  type(r_solver_field_type) :: f3

  call invoke( real_to_real_X(f2, f1), &
               real_to_real_X(f1, f3), &
               real_to_real_X(f3, f2) )

end program single_invoke
