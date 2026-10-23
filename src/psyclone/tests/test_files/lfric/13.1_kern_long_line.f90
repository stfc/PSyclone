! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single function specified in an invoke call
  use longkern_mod,  only: longkern_type
  use field_mod,     only: field_type

  implicit none

  type(field_type) :: f1, f2, m1, m2

  call invoke(                      &
       longkern_type(f1,f2,m1,m2)   &
          )

end program single_invoke
