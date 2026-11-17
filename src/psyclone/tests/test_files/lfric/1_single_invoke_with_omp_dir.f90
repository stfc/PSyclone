! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single function specified in an invoke call. Added directive
  ! and comment to test keep-comments and keep-directives functionality.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_mod,  only: testkern_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a

  !Here is a comment
  call invoke(                          &
       testkern_type(a, f1, f2, m1, m2) &
          )
  !$omp barrier

end program single_invoke
