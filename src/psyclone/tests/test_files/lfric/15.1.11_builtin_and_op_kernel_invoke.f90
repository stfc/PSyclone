! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_builtin_then_kernel

  ! Description: single invoke call with a builtin followed by a kernel call
  ! with an operator argument.
  use constants_mod,        only: r_def
  use field_mod,            only: field_type
  use operator_mod,         only: operator_type
  use dg_matrix_vector_kernel_mod, only: dg_matrix_vector_kernel_type

  implicit none

  type(field_type) :: f2, f4
  real(r_def)      :: scalar = 0.0
  type(operator_type), pointer    :: mass_matrix => null()

  call invoke(                               &
       setval_c(f2, 0.0),                    &
       ! f4 - discont. space, written
       ! f2 - continuous space, read - needs halo exchange to get
       ! clean annexed dofs.
       dg_matrix_vector_kernel_type(f4, f2, mass_matrix) )

end program single_invoke_builtin_then_kernel
