! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Invokes two kernels, neither of which can perform redundant computation
  ! because they have OPERATES_ON = OWNED_CELL_COLUMN/OWNED_DOF.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_owned_cell_mod,  only: testkern_owned_cell_type
  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a

  call invoke( testkern_owned_cell_type(a, f1, f2, m1, m2), &
               setval_random(f1))

end program single_invoke
