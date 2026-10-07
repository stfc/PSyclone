! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_dofs

  ! Description: single user-defined kernel specified in an invoke call that
  ! iterates over DoFs
  use constants_mod,      only: r_def
  use field_mod,          only: field_type
  use testkern_dofs_mod,  only: testkern_dofs_type

  implicit none

  type(field_type) :: f1, f2, f3, f4
  type(field_type) :: field_vec(3)
  real(kind=r_def) :: scalar_arg

  call invoke(                                                   &
       testkern_dofs_type(f1, f2, f3, f4, field_vec, scalar_arg) &
          )

end program single_invoke_dofs
