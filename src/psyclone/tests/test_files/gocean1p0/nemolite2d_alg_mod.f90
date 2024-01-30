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
! Author A. R. Porter, STFC Daresbury Lab

module nemolite2d_alg_mod

contains

subroutine step(istp,           &
                ua, va, un, vn, &
                sshn_t, sshn_u, sshn_v, &
                ssha_t, ssha_u, ssha_v, &
                hu, hv, ht)
  use kind_params_mod
  use grid_mod
  use field_mod
  use model_mod, only: rdt ! The model time-step

  use continuity_mod,  only: continuity
  use momentum_mod,    only: momentum_u
  implicit none
  !> The current time step
  integer,         intent(inout) :: istp
  type(r2d_field), intent(inout) :: un, vn, sshn_t, sshn_u, sshn_v
  type(r2d_field), intent(inout) :: ua, va, ssha_t, ssha_u, ssha_v
  type(r2d_field), intent(inout) :: hu, hv, ht

  call invoke(                                               &
              continuity(ssha_t, sshn_t, sshn_u, sshn_v,     &
                         hu, hv, un, vn, rdt),               &
              momentum_u(ua, un, vn, hu, hv, ht,             &
                         ssha_u, sshn_t, sshn_u, sshn_v)     &
             )

end subroutine step

end module nemolite2d_alg_mod
