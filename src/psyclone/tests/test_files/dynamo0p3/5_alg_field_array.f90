! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2024, Science and Technology Facilities Council
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
! Author R. W. Ford, STFC Daresbury Lab
! Modified I. Kavcic and L. Turner, Met Office

program single_function

  ! Description: field_type arrays indexed in the invoke
  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use testkern_qr_mod,     only: testkern_qr_type

  implicit none

  type(field_type)           :: f0(2), f1(2,2)
  type(quadrature_xyoz_type) :: qr
  real(r_def)                :: b(2), a(8)
  integer(i_def)             :: iflag(4)
  integer(i_def)             :: index, index1, index2, index3

  call invoke(                                                   &
       testkern_qr_type(f0(1), f1(1,1), f1(2,index), b(1),       &
                        f1(index,index2(index3)), iflag(2), qr), &
       testkern_qr_type(f1(index,index2(index3)),                &
                        f1(2,index), f1(1,1), a(index1), f0(1),  &
                        iflag(index2(index3)), qr)               &
          )

end program single_function
