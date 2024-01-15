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

module momentum_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod
  use field_mod
  implicit none

  private

  public momentum_u
  public momentum_u_code

  !=======================================

  type, extends(kernel_type) :: momentum_u
     type(go_arg), dimension(18) :: meta_args =  &
          (/ go_arg(GO_READWRITE, GO_CU, GO_POINTWISE),  & ! ua
             go_arg(GO_READ,      GO_CU, GO_POINTWISE),  & ! un
             go_arg(GO_READ,      GO_CV, GO_POINTWISE),  & ! vn
             go_arg(GO_READ,      GO_CU, GO_POINTWISE),  & ! hu
             go_arg(GO_READ,      GO_CV, GO_POINTWISE),  & ! hv
             go_arg(GO_READ,      GO_CT, GO_POINTWISE),  & ! ht
             go_arg(GO_READ,      GO_CU, GO_POINTWISE),  & ! ssha_u
             go_arg(GO_READ,      GO_CT, GO_POINTWISE),  & ! sshn_t
             go_arg(GO_READ,      GO_CU, GO_POINTWISE),  & ! sshn_u
             go_arg(GO_READ,      GO_CV, GO_POINTWISE),  & ! sshn_v
             go_arg(GO_READ,      GO_GRID_MASK_T),    &
             go_arg(GO_READ,      GO_GRID_DX_U),      &
             go_arg(GO_READ,      GO_GRID_DX_V),      &
             go_arg(GO_READ,      GO_GRID_DX_T),      &
             go_arg(GO_READ,      GO_GRID_DY_U),      &
             go_arg(GO_READ,      GO_GRID_DY_T),      &
             go_arg(GO_READ,      GO_GRID_AREA_U),    &
             go_arg(GO_READ,      GO_GRID_LAT_U)      &
           /)

     integer :: ITERATES_OVER = GO_INTERNAL_PTS
     integer :: index_offset = GO_OFFSET_NE

  contains
    procedure, nopass :: code => momentum_u_code
  end type momentum_u

contains

  subroutine momentum_u_code(ji, jj, &
                             ua, un, vn, &
                             hu, hv, ht, ssha_u, &
                             sshn, sshn_u, sshn_v, &
                             tmask, e1u, e1v, e1t, e2u, e2t, e12u, gphiu)
    implicit none
    integer, intent(in) :: ji, jj
    integer,  dimension(:,:), intent(in) :: tmask
    real(go_wp), dimension(:,:), intent(in) :: e1u, e1v, e1t, e12u, e2u, e2t, gphiu
    real(go_wp), dimension(:,:), intent(in) :: hu, hv, ht
    real(go_wp), dimension(:,:), intent(in) :: ssha_u, sshn, sshn_u, sshn_v
    real(go_wp), dimension(:,:), intent(in) :: un, vn
    real(go_wp), dimension(:,:), intent(out) :: ua

    !kernel ua calculation 
    ua(ji,jj) = 0.0_go_wp

  end subroutine momentum_u_code 

end module momentum_mod
