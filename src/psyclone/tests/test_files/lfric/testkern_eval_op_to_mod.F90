! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_eval_op_to_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  ! Test kernel that writes to an operator and requires an evaluator with
  ! a space corresponding to the 'to' space of the operator.
  type, extends(kernel_type) :: testkern_eval_op_to_type
     type(arg_type)  :: meta_args(2) =  (/               &
       arg_type(GH_OPERATOR, GH_REAL, GH_READ,  W2, W0), &
       arg_type(GH_FIELD,    GH_REAL, GH_WRITE, W3)      &
       /)
     type(func_type) :: meta_funcs(2) = (/               &
       func_type(W2, GH_BASIS, GH_DIFF_BASIS),           &
       func_type(W3, GH_DIFF_BASIS)                      &
       /)
     integer :: operates_on = CELL_COLUMN
     integer :: gh_shape = gh_evaluator
   contains
     procedure, nopass :: code => testkern_eval_op_to_code
  end type testkern_eval_op_to_type

contains

  subroutine testkern_eval_op_to_code(cell, nlayers, ncell_3d,     &
                                      op1_stencil, f2, ndf_w2,     &
                                      basis_w2_on_w3,              &
                                      diff_basis_w2_on_w3, ndf_w0, &
                                      ndf_w3, undf_w3, map_w3,     &
                                      diff_basis_w3_on_w3)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    integer(kind=i_def), intent(in) :: ndf_w2, ndf_w0, undf_w3
    real(kind=r_def), intent(inout), dimension(undf_w3) :: f2
    real(kind=r_def), intent(in), dimension(ncell_3d,ndf_w2,ndf_w0) :: op1_stencil
    real(kind=r_def), intent(in), dimension(3,ndf_w2,ndf_w3) :: basis_w2_on_w3
    real(kind=r_def), intent(in), dimension(1,ndf_w2,ndf_w3) :: diff_basis_w2_on_w3
    real(kind=r_def), intent(in), dimension(3,ndf_w3,ndf_w3) :: diff_basis_w3_on_w3

  end subroutine testkern_eval_op_to_code

end module testkern_eval_op_to_mod
