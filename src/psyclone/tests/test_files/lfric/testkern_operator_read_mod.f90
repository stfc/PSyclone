! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_operator_read_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_operator_read_type
     type(arg_type), dimension(3) :: meta_args =                  &
          (/ arg_type(gh_operator, gh_real,    gh_read,  w3, w3), &
             arg_type(gh_field*3,  gh_real,    gh_write, w3),     &
             arg_type(gh_scalar,   gh_integer, gh_read)           &
          /)
     type(func_type) :: meta_funcs(1) =                           &
          (/ func_type(w3, gh_basis, gh_diff_basis)               &
          /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_operator_read_code
  end type testkern_operator_read_type

contains

  subroutine testkern_operator_read_code(cell, nlayers, ncell_3d, &
                                         local_stencil,           &
                                         xdata, ydata, zdata,     &
                                         iscalar,                 &
                                         ndf_w3, undf_w3, map_w3, &
                                         basis_w3, diff_basis_w3, &
                                         np_xy, np_z, weights_xy, weights_z)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: iscalar
    integer(kind=i_def), intent(in) :: np_xy, np_z
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def), intent(inout), dimension(undf_w3) :: xdata
    real(kind=r_def), intent(inout), dimension(undf_w3) :: ydata
    real(kind=r_def), intent(inout), dimension(undf_w3) :: zdata
    real(kind=r_def), intent(in), dimension(ncell_3d,ndf_w3,ndf_w3) :: local_stencil
    real(kind=r_def), intent(in), dimension(1,ndf_w3,np_xy,np_z) :: basis_w3
    real(kind=r_def), intent(in), dimension(3,ndf_w3,np_xy,np_z) :: diff_basis_w3
    real(kind=r_def), intent(in), dimension(np_xy) :: weights_xy
    real(kind=r_def), intent(in), dimension(np_z)  :: weights_z

  end subroutine testkern_operator_read_code

end module testkern_operator_read_mod
