! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! This simple module is used to showcase and test the extraction of non-local
! module variables with the driver extraction.

module dummy_mod
  integer :: dummy_var1
  real :: dummy_var2
  real :: dummy_var3 = 3

  public :: dummy_code

  interface dummy_code
   module procedure dummy_code_1, dummy_code_2
  end interface

  contains

  subroutine dummy_code_1(a)
    implicit none
    integer :: a
    dummy_var1 = dummy_var1 + 1
  end subroutine dummy_code_1

  subroutine dummy_code_2(a)
    implicit none
    real :: a
    dummy_var1 = dummy_var1 + 1
  end subroutine dummy_code_2

  integer function dummy_func(a)
    implicit none
    integer :: a
    dummy_func = a+1 + dummy_var2
  end function dummy_func

end module dummy_mod
