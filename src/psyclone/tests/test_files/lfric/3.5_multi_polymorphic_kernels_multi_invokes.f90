! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2024, Science and Technology Facilities Council
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
! Author: A. R. Porter STFC Daresbury Lab

program multi_functions_multi_invokes

  ! Description: multiple invoke calls, each involving the same
  ! polymorphic kernel.
  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use mixed_kernel_mod,    only: mixed_kernel_type
  use testkern_qr_mod,     only: testkern_qr_type

  implicit none

  type(field_type)             :: f1, f2, m1, m2
  type(field_type)             :: fieLd_r_def
  type(r_phys_field_type)      :: fiEld_r_phys
  type(quadrature_xyoz_type)   :: qr
  type(operator_type)          :: operator_r_def
  real(r_def)                  :: a
  integer(i_def)               :: istp
  real(r_def)                  :: Scalar_r_def
  real(r_phys)                 :: scalAr_r_phys

  call invoke(                                       &
       mixed_kernel_type(scalar_r_deF, field_R_def, opeRator_r_def), &
       testkern_qr_type(f1, f2, m1, a, m2, istp, qr) &
       )

  call invoke(                                        &
       mixed_kernel_type(scaLar_r_phys, fIeld_r_phys, opeRator_r_def), &
       testkern_qr_type(f1, f2, m1, a, m2, istp, qr)  &
       )

end program multi_functions_multi_invokes
