
! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

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
