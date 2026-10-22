! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: one reduction builtin followed by an access to 
  ! the reduction value.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_write_any_w2trace_mod, only: testkern_write_any_w2trace_type
  use testkern_stencil_w3_mod, only: testkern_stencil_w3_type
  implicit none

  type(field_type) :: f1, f2
  type(field_type) :: w1, w2, w3, w4, w5, w6, w7, w8
  integer(i_def)   :: f2_extent = 2
  real(r_def)      :: asum

  call invoke( setval_c(f1, asum), &
               setval_c(f2, asum), &
               testkern_write_any_w2trace_type(w1, w2, w3, f1, w4, w5, w6, w7), &
               testkern_stencil_w3_type(f2, w8, f2_extent) &
           )

end program single_invoke
