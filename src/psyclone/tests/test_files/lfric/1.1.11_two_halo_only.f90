!-------------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2024-2025, Science and Technology Facilities Council
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
! Author: A. R. Porter, STFC Daresbury Lab

program single_invoke

  ! Description: an invoke containing multiple calls to a kernel that requires
  ! XYoZ quadrature and that operates on halo cells to a depth specified in
  ! the invoke call.
  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use testkern_qr_and_halo_only_mod, only: testkern_qr_and_halo_only_type

  implicit none

  type(field_type)           :: f1, f2, m1, m2
  type(quadrature_xyoz_type) :: qr
  real(r_def)                :: a, b
  integer(i_def)             :: istp, hdepth, hdepth2

  call invoke(                                       &
       testkern_qr_and_halo_only_type(f1, f2, m1, a, m2, istp, qr, hdepth), &
       testkern_qr_and_halo_only_type(f1, f2, m1, b, m2, istp, qr, hdepth), &
       testkern_qr_and_halo_only_type(f2, f1, m1, a, m2, istp, qr, hdepth2) &
          )

end program single_invoke
