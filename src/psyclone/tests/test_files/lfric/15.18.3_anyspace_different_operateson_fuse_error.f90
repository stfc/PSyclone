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
  use non_dof_anyspace_mod, only: non_dof_anyspace_kern
  implicit none

  type(field_type) :: f1
  real(r_def)      :: asum

  call invoke( non_dof_anyspace_kern(f1, asum), &
               setval_c(f1, asum) )

end program single_invoke
