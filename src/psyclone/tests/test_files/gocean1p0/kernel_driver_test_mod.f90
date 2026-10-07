! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> \brief Simple test case for extract driver generation.
!! \detail This kernel has all type of variable accesses:
!! input, input+output, output

module kernel_driver_test_mod
  use argument_mod
  use field_mod
  use grid_mod
  use kind_params_mod
  use kernel_mod
  implicit none

  private

  public compute_kernel_code

  type, extends(kernel_type) :: compute_kernel
     type(go_arg), dimension(6) :: meta_args =    &
          (/ go_arg(GO_WRITE,     GO_CU, GO_POINTWISE),   &
             go_arg(GO_READWRITE, GO_CT, GO_POINTWISE),   &
             go_arg(GO_READ,      GO_CU, GO_POINTWISE),   &
             go_arg(GO_READ,      GO_CU, GO_POINTWISE),   &
             go_Arg(GO_READ,      GO_GRID_DX_CONST),      &
             go_Arg(GO_READ,      GO_GRID_LAT_U)          &
           /)
     integer :: ITERATES_OVER = GO_INTERNAL_PTS
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_kernel_code
  end type compute_kernel

contains

  !===================================================
  subroutine compute_kernel_code(i, j, out_f, inout_f, in_f, &
                                 dummy_field, dx_const, lat_u)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(out),   dimension(:,:) :: out_f
    real(go_wp), intent(inout), dimension(:,:) :: inout_f
    real(go_wp), intent(in),    dimension(:,:) :: in_f
    real(go_wp), intent(in),    dimension(:,:) :: dummy_field
    real(go_wp), intent(in)                    :: dx_const
    real(go_wp), intent(in),    dimension(:,:) :: lat_u

    out_f(i,j) = in_f(i,j) + dx_const * lat_u(i,j)
    inout_f(i,j) = inout_f(i,j) + in_f(i,j)*dummy_field(i,j)

  end subroutine compute_kernel_code

end module kernel_driver_test_mod
