! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module assemble_weak_derivative_w3_w2_kernel_mod

  use kernel_mod,            only : kernel_type
  use constants_mod,         only : r_def, i_def
  use fs_continuity_mod,     only : W3, W2, W0
  use argument_mod,          only : arg_type, func_type,        &
                                    GH_FIELD, GH_OPERATOR,      &
                                    GH_REAL, GH_WRITE, GH_READ, &
                                    GH_BASIS, GH_DIFF_BASIS,    &
                                    CELL_COLUMN, gh_quadrature_XYoZ

  implicit none

  private

  type, public, extends(kernel_type) :: assemble_weak_derivative_w3_w2_kernel_type
    private
    type(arg_type) :: meta_args(2) = (/                        &
         ! Operator maps *to* FS W3 *from* FS W2
         arg_type(GH_OPERATOR, GH_REAL, GH_WRITE, W3, W2),     &
         arg_type(GH_FIELD*3,  GH_REAL, GH_READ,  W0)          &
         /)
    type(func_type) :: meta_funcs(3) = (/                      &
         func_type(W0, GH_DIFF_BASIS),                         &
         func_type(W3, GH_BASIS),                              &
         func_type(W2, GH_DIFF_BASIS)                          &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = gh_quadrature_XYoZ
  contains
    procedure, nopass :: assemble_weak_derivative_w3_w2_kernel_code
  end type

  public assemble_weak_derivative_w3_w2_kernel_code

contains
!
  subroutine assemble_weak_derivative_w3_w2_kernel_code(cell, nlayers, ncell_3d,    &
                                            local_stencil, xdata, ydata, zdata,     &
                                            ndf_w3, basis_w3,                       &
                                            ndf_w2, diff_basis_w2,                  &
                                            ndf_w0, undf_w0, map_w0, diff_basis_w0, &
                                            np_xy, np_z, weights_xy, weights_z)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_w3, ndf_w2, undf_w0
    integer(kind=i_def), intent(in) :: np_xy, np_z
    integer(kind=i_def), intent(in), dimension(ndf_w0) :: map_w0
    real(kind=r_def), intent(inout), dimension(ncell_3d,ndf_w3,ndf_w2) :: local_stencil
    real(kind=r_def), intent(in), dimension(undf_w0) :: xdata
    real(kind=r_def), intent(in), dimension(undf_w0) :: ydata
    real(kind=r_def), intent(in), dimension(undf_w0) :: zdata
    real(kind=r_def), intent(in), dimension(3,ndf_w0,np_xy,np_z) :: diff_basis_w0
    real(kind=r_def), intent(in), dimension(1,ndf_w3,np_xy,np_z) :: basis_w3
    real(kind=r_def), intent(in), dimension(1,ndf_w2,np_xy,np_z) :: diff_basis_w2
    real(kind=r_def), intent(in), dimension(np_xy) :: weights_xy
    real(kind=r_def), intent(in), dimension(np_z)  :: weights_z

  end subroutine assemble_weak_derivative_w3_w2_kernel_code
!
end module assemble_weak_derivative_w3_w2_kernel_mod
