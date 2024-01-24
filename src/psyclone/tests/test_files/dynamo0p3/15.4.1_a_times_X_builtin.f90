! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
! Author: A. R. Porter, STFC Daresbury Lab
! Modified: I. Kavcic, Met Office

program single_invoke

  ! Description: single point-wise operation (scale a field: Y = aX) on
  ! real-valued fields and a real scalar of precision 'r_phys' specified
  ! in an invoke call (the default precision is 'r_def').
  use constants_mod,    only: r_phys
  use r_phys_field_mod, only: r_phys_field_type

  implicit none

  type(r_phys_field_type) :: f1, f2
  real(r_phys)            :: a_scalar

  a_scalar = 2.0_r_phys

  call invoke( a_times_X(f2, a_scalar, f1) )

end program single_invoke
