! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2025, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
!-------------------------------------------------------------------------------
! Author A. Pirrie, Met Office

program scalar_array_invoke

  ! Description: single function specified in an invoke call to test the
  ! ScalarArray functionality
  use constants_mod,                 only: i_def, r_def, l_def
  use field_mod,                     only: field_type
  use testkern_scalar_array_mod,     only: testkern_scalar_array_type
  use testkern_two_int_scalars_mod,  only: testkern_two_int_scalars_type

  implicit none

  type(field_type)                            :: f1, f2, f3, f4
  real(kind=r_def),    dimension(50, 100)     :: real_array
  logical(kind=l_def), dimension(10)          :: logical_array
  integer(kind=i_def), dimension(2, 5, 10, 8) :: integer_array
  integer(kind=i_def)                         :: a, b, dims_integer_array

  ! Include dims_integer_array as a scalar value to check that the
  ! generated code names do not clash
  call invoke(                                                                                       &
       testkern_scalar_array_type(f1, real_array, logical_array, integer_array, dims_integer_array), &
       testkern_two_int_scalars_type(a, f1, f2, f3, f4, b)                                           &
          )

end program scalar_array_invoke
