! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2024, Science and Technology Facilities Council
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
! Modified I. Kavcic Met Office

module testkern_any_space_4_mod

  use argument_mod
  use kernel_mod
  use constants_mod, only : r_def, i_def

  implicit none

  ! Test for any_space producing correct code with different
  ! permutations of whether ANY_SPACE is used by another operator/field
  ! or not and whether it has a basis function or not
  type, public, extends(kernel_type) :: testkern_any_space_4_type
    private
    type(arg_type) :: meta_args(6) = (/                             &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,      ANY_SPACE_5), &
         arg_type(GH_OPERATOR, GH_REAL, GH_READWRITE, ANY_SPACE_1,  &
                                                      ANY_SPACE_2), &
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,      ANY_SPACE_3,  &
                                                      ANY_SPACE_2), &
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,      ANY_SPACE_4,  &
                                                      ANY_SPACE_4), &
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,      ANY_SPACE_3,  &
                                                      ANY_SPACE_5), &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,      ANY_SPACE_4)  &
         /)
    type(func_type) :: meta_funcs(2) = (/                           &
         func_type(ANY_SPACE_1, GH_BASIS),                          &
         func_type(ANY_SPACE_4, GH_BASIS, GH_DIFF_BASIS)            &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = gh_quadrature_XYoZ
  contains
    procedure, public, nopass :: testkern_any_space_4_code
  end type testkern_any_space_4_type

contains
  
  subroutine testkern_any_space_4_code(cell, nlayers, adata,                    &
                                       ncell_3d_b, b_stencil,                   &
                                       ncell_3d_c, c_stencil,                   &
                                       ncell_3d_d, d_stencil,                   &
                                       ncell_3d_e, e_stencil, fdata,            &
                                       ndf_aspc5_a, undf_aspc5_a, map_aspc5_a,  &
                                       ndf_aspc1_b, basis_aspc1_b_qr,           &
                                       ndf_aspc2_b, ndf_aspc3_c,                &
                                       ndf_aspc4_d, undf_aspc4_d, map_aspc4_d,  &
                                       basis_aspc4_d_qr, diff_basis_aspc4_d_qr, &
                                       np_xy, np_z, weights_xy, weights_z)

    implicit none

    integer(kind=i_def) :: cell, nlayers
    integer(kind=i_def) :: ncell_3d_b, ncell_3d_c, ncell_3d_d, ncell_3d_e
    integer(kind=i_def) :: ndf_aspc5_a, undf_aspc5_a, &
                           ndf_aspc1_b, ndf_aspc2_b,  &
                           ndf_aspc3_c, ndf_aspc4_d, undf_aspc4_d
    integer(kind=i_def) :: np_xy, np_z
    integer(kind=i_def), dimension(ndf_aspc5_a) :: map_aspc5_a
    integer(kind=i_def), dimension(ndf_aspc4_d) :: map_aspc4_d
    real(kind=r_def), dimension(undf_aspc5_a) :: adata
    real(kind=r_def), dimension(undf_aspc4_d) :: fdata
    real(kind=r_def), dimension(ndf_aspc1_b,ndf_aspc2_b,ncell_3d_b) :: b_stencil
    real(kind=r_def), dimension(ndf_aspc3_c,ndf_aspc2_b,ncell_3d_c) :: c_stencil
    real(kind=r_def), dimension(ndf_aspc4_d,ndf_aspc4_d,ncell_3d_d) :: d_stencil
    real(kind=r_def), dimension(ndf_aspc3_c,ndf_aspc5_a,ncell_3d_e) :: e_stencil
    real(kind=r_def), dimension(:,:,:,:) :: basis_aspc1_b_qr, &
                                            basis_aspc4_d_qr, &
                                            diff_basis_aspc4_d_qr
    real(kind=r_def), dimension(:) :: weights_xy, weights_z

  end subroutine testkern_any_space_4_code

end module testkern_any_space_4_mod
