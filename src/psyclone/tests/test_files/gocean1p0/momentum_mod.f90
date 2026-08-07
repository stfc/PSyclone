! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

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
