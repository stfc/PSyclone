! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_annexed

  ! Description: f1 and f2 are written to by intrinsics and
  ! then read. f1 is on the w1 function space and f2 is on the w2
  ! function space, so both are continuous and therefore have annexed
  ! dofs. By default the intrinsics only write to owned
  ! dofs. Therefore a halo exchange will be required so that the
  ! annexed dofs for both f1 and f2 are clean when they are read.
  use constants_mod,   only: r_def
  use field_mod,       only: field_type
  use testkern_w3_mod, only: testkern_w3_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a

  call invoke(                             &
       setval_c(f1, 0.0),                  &
       setval_c(f2, 0.0),                  &
       testkern_w3_type(a, f1, f2, m1, m2) &
          )

end program single_invoke_annexed
