! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_cell_dof_builtin

  ! Description: Three user-defined kernels operating over different domains
  ! ("cell-column" and "dof"), followed by an LFRic built-in kernel in the
  ! same invoke.
  use constants_mod,      only: r_def
  use field_mod,          only: field_type
  use testkern_dofs_mod,  only: testkern_dofs_type
  use testkern_dofs_vector_write_mod,  only: testkern_dofs_vector_write_type
  use testkern_mod,       only: testkern_type

  implicit none

  type(field_type) :: f1, f2, f3, f4, m1, m2
  type(field_type) :: field_vec(3)
  real(kind=r_def) :: a, scalar_arg

  call invoke(                                                    &
       testkern_dofs_vector_write_type(field_vec),                &
       testkern_dofs_type(f1, f2, f3, f4, field_vec, scalar_arg), &
       testkern_type(a, f1, f2, m1, m2),                          &
       inc_aX_plus_Y(0.5_r_def, f1, f2)                           &
          )

end program multikernel_invokes_cell_dof_builtin
