! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> Test kernel that purports to write to both a field and an operator.
module testkern_write_op_and_fld_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_write_op_and_fld_type
     type(arg_type), dimension(3) :: meta_args =                 &
          (/ arg_type(gh_field*3,  gh_real,    gh_write, w3),    &
             arg_type(gh_scalar,   gh_integer, gh_read),         &
             arg_type(gh_operator, gh_real,    gh_write, w0, w0) &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_write_op_and_fld_code
  end type testkern_write_op_and_fld_type

contains

  subroutine testkern_write_op_and_fld_code(cell, nlayers,                   &
                                            field1_v1, field1_v2, field1_v3, &
                                            iscalar, ncell_3d, op,           &
                                            ndf_w3, undf_w3, map_w3, ndf_w0)
    implicit none

    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: iscalar
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def), intent(inout), dimension(undf_w3) :: field1_v1
    real(kind=r_def), intent(inout), dimension(undf_w3) :: field1_v2
    real(kind=r_def), intent(inout), dimension(undf_w3) :: field1_v3
    real(kind=r_def), intent(inout), dimension(ndf_w0,ndf_w0,ncell_3d) :: op

  end subroutine testkern_write_op_and_fld_code

end module testkern_write_op_and_fld_mod
