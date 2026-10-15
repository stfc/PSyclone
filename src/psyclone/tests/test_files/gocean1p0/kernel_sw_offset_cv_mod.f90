! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

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
