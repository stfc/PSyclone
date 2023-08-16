!-------------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2023, Science and Technology Facilities Council
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
! Author: R. W. Ford, STFC Daresbury Lab
! Modified: I. Kavcic and L. Turner, Met Office

program single_invoke

  ! Description: single function specified in an invoke call where the
  ! quadrature is supplied by dereferencing a derived type. Note, none of
  ! the actual LFRic objects has quadrature so we use a fake reference-
  ! element-like object that contains a quadrature object.
  use constants_mod,         only: r_def, i_def
  use reference_element_mod, only: reference_element_type
  use field_mod,             only: field_type
  use testkern_qr_mod,       only: testkern_qr_type

  implicit none

  type(field_type)             :: f1, f2, m1, m2
  type(reference_element_type) :: unit_cube
  real(r_def)                  :: a
  integer(i_def)               :: istp

  call invoke( testkern_qr_type(f1, f2, m1, a, m2, istp, unit_cube%qr_XYoZ) )

end program single_invoke
