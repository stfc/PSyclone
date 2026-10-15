! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_builtin_then_kernel

  ! Description: single invoke call with a builtin followed by a kernel call
  use constants_mod,        only: r_def
  use field_mod,            only: field_type
  use testkern_mod,         only: testkern_type
  use testkern_wtheta_mod,  only: testkern_wtheta_type
  use testkern_w2_only_mod, only: testkern_w2_only_type

  implicit none

  type(field_type) :: f1, f2, f3, f4, f5
  real(r_def)      :: scalar = 0.0
  
  call invoke(                               &
       setval_c(f5, 0.0),                    &
       setval_c(f2, 0.0),                    &
       ! f3 function space w2, inc
       ! f2 function space w2, read
       testkern_w2_only_type(f3, f2),        &
       ! f4 function space wtheta, write
       ! f5 function space any_discontinuous_space_1, read
       testkern_wtheta_type(f4, f5),         &
       ! scalar, read
       ! f1 function space w1, inc
       ! f2 function space w2, read
       ! f3 function space w2, read
       ! f4 function space w3, read
       testkern_type(scalar, f1, f2, f3, f4) &
          )

end program single_invoke_builtin_then_kernel
