! BSD 3-Clause License
!
! Copyright (c) 2023-2024, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author J. Henrichs, Bureau of Meteorology

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
