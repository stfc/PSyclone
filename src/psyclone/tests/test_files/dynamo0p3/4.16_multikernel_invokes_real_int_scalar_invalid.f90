! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2021-2024, Science and Technology Facilities Council
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
! -----------------------------------------------------------------------------
! Author: I. Kavcic, Met Office
! Modified: R. W. Ford, STFC Daresbury Lab

program multikernel_invokes_real_int_scalar_invalid

  ! Description: two kernel calls with the first kernel using two real
  ! scalars and the second kernel using two integer scalars but
  ! incorrectly passing a scalar that would need to be both real and
  ! integer. This is provided from a module to stop PSyclone's type
  ! checking from raising an exception.

  use constants_mod,                 only: r_def, i_def
  use field_mod,                     only: field_type
  use testkern_two_real_scalars_mod, only: testkern_two_real_scalars_type
  use testkern_two_int_scalars_mod,  only: testkern_two_int_scalars_type

  use unknown_mod, only : b

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a
  integer(i_def)   :: iflag

  call invoke(name = "real_and_integer_scalars",               &
       testkern_two_real_scalars_type(a, f1, f2, m1, m2, b),   &
       testkern_two_int_scalars_type(iflag, f1, f2, m1, m2, b) &
             )

end program multikernel_invokes_real_int_scalar_invalid
