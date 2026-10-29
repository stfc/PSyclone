! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------


SUBROUTINE test27_loop_swap()

  use field_mod
  use boundary_conditions_ne_offset_mod, only : bc_ssh, bc_solid_u, bc_solid_v
  implicit none

  type(r2d_field) :: t, u, v
  ! Those three functions all create different i/j loop boundaries
  ! which simplifies testing.
  call invoke( name="loop1",  &
       bc_ssh(1, t),          &
       bc_solid_u(u),         &
       bc_solid_v(v)            )

  call invoke( name="loop2",  &
       bc_ssh(1, t),          &
       bc_ssh(1, t)             )
END SUBROUTINE test27_loop_swap
