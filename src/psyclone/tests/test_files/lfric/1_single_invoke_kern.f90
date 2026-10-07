! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Description: single function specified in an invoke call with the
! kernel in a different directory to this algorithm file to test the
! -d option of the psyclone script.

program single_invoke_kern

  use constants_mod,      only: r_def
  use field_mod,          only: field_type
  use testkern_dir_mod,   only: testkern_dir_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a

  call invoke(                              &
       testkern_dir_type(a, f1, f2, m1, m2) &
          )

end program single_invoke_kern
