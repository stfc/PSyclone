! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single function specified in an invoke call where a field
  ! object is passed by dereferencing an "estate_type" object.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_mod,  only: testkern_type

  implicit none

  type estate_type
     type(field_type) :: f2,m2
  end type estate_type
  
  type(field_type)  :: f1, m1
  type(estate_type) :: est
  real(r_def)       :: a

  call invoke( testkern_type(a, f1, est%f2, m1, est%m2) )

end program single_invoke
