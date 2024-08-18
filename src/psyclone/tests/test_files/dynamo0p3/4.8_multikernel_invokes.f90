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
! -----------------------------------------------------------------------------
! Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

program multikernel_invokes_7

  ! Multiple kernel calls within an invoke where the fields updated by
  ! the two kernels are on different spaces

  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use ru_kernel_mod,       only: ru_kernel_type
  use testkern_mod,        only: testkern_type

  implicit none

  type(field_type)           :: a, b, c, d, e(3), f, g, h
  real(r_def)                :: ascalar, rdt
  integer(i_def)             :: istp
  type(quadrature_xyoz_type) :: qr

  call invoke(                                            &
               ! h is written, rest are read-only
               testkern_type(rdt, h, f, c, d),            &
               ! b is written, rest are read-only
               testkern_type(rdt, b, f, c, d),            &
               ! b is gh_inc, rest are read-only
               ru_kernel_type(b, a, istp, rdt, c, e, qr), &
               ! g is gh_inc, rest are read-only
               ru_kernel_type(g, a, istp, rdt, c, e, qr), &
               ! f is written, rest are read-only
               testkern_type(ascalar, f, b, c, d) )

  ! => b and h must be intent(inout)
  ! => g and f must be intent(inout)
  ! => a, c, d and e are intent(in)
end program multikernel_invokes_7
