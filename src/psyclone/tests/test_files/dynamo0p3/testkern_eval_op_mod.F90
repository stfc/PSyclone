!-------------------------------------------------------------------------------
! Copyright (c) 2017-2024, Science and Technology Facilities Council
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
!-------------------------------------------------------------------------------
! Author: A. R. Porter STFC Daresbury Lab
! Modified: I. Kavcic Met Office

module testkern_eval_op_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  ! Test kernel that writes to an operator and requires an evaluator
  type, extends(kernel_type) :: testkern_eval_op_type
     type(arg_type)  :: meta_args(2) =  (/               &
       arg_type(GH_OPERATOR, GH_REAL, GH_WRITE, W0, W2), &
       arg_type(GH_FIELD,    GH_REAL, GH_READ,  W3)      &
       /)
     type(func_type) :: meta_funcs(2) = (/               &
       func_type(W2, GH_BASIS),                          &
       func_type(W3, GH_DIFF_BASIS)                      &
       /)
     integer :: operates_on = CELL_COLUMN
     integer :: gh_shape = gh_evaluator
   contains
     procedure, nopass :: code => testkern_eval_op_code
  end type testkern_eval_op_type

contains

  subroutine testkern_eval_op_code(cell, nlayers, ncell_3d, &
                                   op1_stencil, f2, ndf_w0, &
                                   ndf_w2, basis_w2_on_w0,  &
                                   ndf_w3, undf_w3, map_w3, &
                                   diff_basis_w3_on_w0)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: ndf_w0, ndf_w2, undf_w3
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def), intent(in), dimension(undf_w3) :: f2
    real(kind=r_def), intent(inout), dimension(ndf_w0,ndf_w2,ncell_3d) :: op1_stencil
    real(kind=r_def), intent(in), dimension(3,ndf_w2,ndf_w0) :: basis_w2_on_w0
    real(kind=r_def), intent(in), dimension(3,ndf_w3,ndf_w0) :: diff_basis_w3_on_w0

  end subroutine testkern_eval_op_code

end module testkern_eval_op_mod
