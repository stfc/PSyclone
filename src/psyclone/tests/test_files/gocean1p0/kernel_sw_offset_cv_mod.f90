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

module kernel_sw_offset_cv_mod
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  private

  public compute_v, compute_v_code
  public apply_bcs_v, apply_bcs_v_code

  type, extends(kernel_type) :: compute_v
     type(go_arg), dimension(3) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CV, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CT, GO_POINTWISE)         &
           /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_v_code
  end type compute_v

  type, extends(kernel_type) :: apply_bcs_v
     type(go_arg), dimension(2) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CV, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE)         &
           /)
     !> This kernel writes to all points of the
     !! simulation domain.
     integer :: ITERATES_OVER = GO_ALL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => apply_bcs_v_code
  end type apply_bcs_v

contains

  !===================================================

  SUBROUTINE compute_v_code(i, j, v, u, h)
    IMPLICIT none
    integer,  intent(in) :: I, J
    REAL(go_wp), INTENT(inout), DIMENSION(:,:) :: v
    REAL(go_wp), INTENT(in),    DIMENSION(:,:) :: u, h

    v(I,J) = h(I,J)+.25d0*(U(I+1,J)*U(I+1,J)+U(I,J)*U(I,J) + & 
                           V(I,J+1)*V(I,J+1)+V(I,J)*V(I,J))

  END SUBROUTINE compute_v_code

  !===================================================

  SUBROUTINE apply_bcs_v_code(i, j, v, u)
    IMPLICIT none
    integer,  intent(in) :: I, J
    REAL(go_wp), INTENT(inout), DIMENSION(:,:) :: v
    REAL(go_wp), INTENT(in),    DIMENSION(:,:) :: u

    v(I,J) = .25d0*(U(I,J)*U(I,J)+U(I,J)*U(I,J) + & 
              V(I,J)*V(I,J)+V(I,J)*V(I,J))

  END SUBROUTINE apply_bcs_v_code

END MODULE kernel_sw_offset_cv_mod
