! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program driver
  ! Minimal driver for compilation testing
  use helmholtz_solver_alg_mod, only : apply_helmholtz_lhs
  use field_mod, only : field_type
  type(field_type) :: Hp, p
  call apply_helmholtz_lhs(Hp,p)
end program driver
