! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_anyw2_operator_mod

  use argument_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, public, extends(kernel_type) :: testkern_anyw2_operator_type
    private
    type(arg_type), dimension(4) :: meta_args = (/                 &
         arg_type(gh_operator, gh_real, gh_write, any_w2, any_w2), &
         arg_type(gh_field,    gh_real, gh_read,  any_w2),         &
         arg_type(gh_field,    gh_real, gh_read,  any_w2),         &
         arg_type(gh_field,    gh_real, gh_read,  any_w2)          &
         /)
    type(func_type) :: meta_funcs(1) =  (/                         &
         func_type(any_w2, gh_basis)                               &
         /)
    integer :: operates_on = cell_column
    integer :: gh_shape = gh_quadrature_XYoZ
  contains
    procedure, nopass :: code => testkern_anyw2_operator_code
  end type testkern_anyw2_operator_type

contains

  subroutine testkern_anyw2_operator_code(cell, nlayers, ncell_3d,      &
                                          op_1, field2, field3, field4, &
                                          ndf_any_w2, undf_any_w2,      &
                                          map_any_w2,                   &
                                          basis_any_w2_qr_xyoz,         &
                                          np_xy_qr_xyoz, np_z_qr_xyoz,  &
                                          weights_xy_qr_xyoz, weights_z_qr_xyoz)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_any_w2
    integer(kind=i_def), intent(in) :: undf_any_w2
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: np_xy_qr_xyoz, np_z_qr_xyoz
    integer(kind=i_def), intent(in), dimension(ndf_any_w2) :: map_any_w2
    real(kind=r_def), intent(in), dimension(undf_any_w2) :: field2
    real(kind=r_def), intent(in), dimension(undf_any_w2) :: field3
    real(kind=r_def), intent(in), dimension(undf_any_w2) :: field4
    real(kind=r_def), intent(inout), dimension(ncell_3d,ndf_any_w2,ndf_any_w2) :: op_1
    real(kind=r_def), intent(in), dimension(3,ndf_any_w2,np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_any_w2_qr_xyoz
    real(kind=r_def), intent(in), dimension(np_xy_qr_xyoz) :: weights_xy_qr_xyoz
    real(kind=r_def), intent(in), dimension(np_z_qr_xyoz) :: weights_z_qr_xyoz

  end subroutine testkern_anyw2_operator_code

end module testkern_anyw2_operator_mod
