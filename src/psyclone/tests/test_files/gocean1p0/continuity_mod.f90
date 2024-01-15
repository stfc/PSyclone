
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

module continuity_mod
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  type, extends(kernel_type) :: continuity
     type(go_arg), dimension(10) :: meta_args =         &
          (/ go_arg(GO_WRITE, GO_CT,       GO_POINTWISE),        & ! ssha
             go_arg(GO_READ,  GO_CT,       GO_POINTWISE),        & ! sshn
             go_arg(GO_READ,  GO_CU,       GO_POINTWISE),        & ! sshn_u
             go_arg(GO_READ,  GO_CV,       GO_POINTWISE),        & ! sshn_v
             go_arg(GO_READ,  GO_CU,       GO_POINTWISE),        & ! hu
             go_arg(GO_READ,  GO_CV,       GO_POINTWISE),        & ! hv
             go_arg(GO_READ,  GO_CU,       GO_POINTWISE),        & ! un
             go_arg(GO_READ,  GO_CV,       GO_POINTWISE),        & ! vn
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE),        & ! Time-step
             go_arg(GO_READ,  GO_GRID_AREA_T)                 &
           /)

     integer :: ITERATES_OVER = GO_INTERNAL_PTS
     integer :: index_offset = GO_OFFSET_NE

  contains
    procedure, nopass :: code => continuity_code
  end type continuity

contains

  subroutine continuity_code(ji, jj,                     &
                             ssha, sshn, sshn_u, sshn_v, &
                             hu, hv, un, vn, rdt, e12t)
    implicit none
    integer,                     intent(in)  :: ji, jj
    real(go_wp),                 intent(in)  :: rdt
    real(go_wp), dimension(:,:), intent(in)  :: e12t
    real(go_wp), dimension(:,:), intent(out) :: ssha
    real(go_wp), dimension(:,:), intent(in)  :: sshn, sshn_u, sshn_v, &
                                             hu, hv, un, vn
    ssha(ji,jj) = 0.0_go_wp

  end subroutine continuity_code

end module continuity_mod
