! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017, Science and Technology Facilities Council
! All rights reserved.
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
! -----------------------------------------------------------------------------
! Authors: A. R. Porter and  R. W. Ford, STFC Daresbury Lab

module testkern_any_space_4_mod
  use argument_mod
  use kernel_mod
  use constants_mod
! test for any_space producing correct code with different
! permutations of whether ANY_SPACE is used by another operator/field
! or not and whether it has a basis function or not

type, public, extends(kernel_type) :: testkern_any_space_4_type
  type(arg_type) :: meta_args(6) = (/                                  &
       arg_type(GH_FIELD, GH_READ, ANY_SPACE_5),                       &
       arg_type(GH_OPERATOR, GH_INC, ANY_SPACE_1, ANY_SPACE_2),        &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_3, ANY_SPACE_2),       &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_4, ANY_SPACE_4),       &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_3, ANY_SPACE_5),       &
       arg_type(GH_FIELD, GH_READ, ANY_SPACE_4)                        &
       /)
  type(func_type) :: meta_funcs(2) = (/                                &
       func_type(ANY_SPACE_1, GH_BASIS),                               &
       func_type(ANY_SPACE_4, GH_BASIS, GH_DIFF_BASIS)                 &
       /)
  integer :: iterates_over = CELLS
  integer :: gh_shape = gh_quadrature_XYoZ
contains
  procedure, public, nopass :: testkern_any_space_4_code
end type testkern_any_space_4_type
!
contains
  
  subroutine testkern_any_space_4_code(cell, nlayers, adata,                &
       ncell_3d_b, b_stencil, ncell_3d_c, c_stencil, ncell_3d_d, d_stencil, &
       ncell_3d_e, e_stencil, fdata, ndf_any_space_5_a, undf_any_space_5_a, &
       map_any_space_5_a, ndf_any_space_1_b, basis_any_space_1_b_qr,        &
       ndf_any_space_2_b, ndf_any_space_3_c, ndf_any_space_4_d,             &
       undf_any_space_4_d, map_any_space_4_d,                               &
       basis_any_space_4_d_qr, diff_basis_any_space_4_d_qr,                 &
       np_xy, np_z, weights_xy, weights_z)
    implicit none
    integer :: cell, nlayers, ncell_3d_b, ncell_3d_c, ncell_3d_d, ncell_3d_e
    integer :: ndf_any_space_5_a, undf_any_space_5_a, ndf_any_space_1_b, &
         ndf_any_space_2_b, ndf_any_space_3_c, ndf_any_space_4_d,        &
         undf_any_space_4_d, np_xy, np_z
    integer, dimension(:) :: map_any_space_5_a, map_any_space_4_d
    real(kind=r_def), dimension(:) :: adata, fdata, weights_xy, weights_z
    real(kind=r_def), dimension(:,:,:,:) :: basis_any_space_1_b_qr, &
         basis_any_space_4_d_qr, diff_basis_any_space_4_d_qr
    real(kind=r_def), dimension(:,:,:) :: b_stencil, c_stencil, d_stencil, &
         e_stencil
  end subroutine testkern_any_space_4_code
!
end module testkern_any_space_4_mod
