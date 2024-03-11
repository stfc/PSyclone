! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2019-2024, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author I. Kavcic, Met Office

module testkern_any_discontinuous_space_op_2_mod

  use constants_mod
  use argument_mod
  use kernel_mod

  implicit none

  ! Description: test for any_discontinuous_space producing correct code
  ! when there are
  ! 1) multiple declarations of the same any_discontinuous_space,
  ! 2) other any_discontinuous_space spaces in the arguments,
  ! 3) any_discontinuous_space used with an operator,
  ! 3) basis and diff_basis functions declared.

  type, public, extends(kernel_type) :: testkern_any_discontinuous_space_op_2_type
    private
    type(arg_type) :: meta_args(3) = (/                                           &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,      ANY_DISCONTINUOUS_SPACE_4), &
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,      ANY_DISCONTINUOUS_SPACE_1,  &
                                                      ANY_DISCONTINUOUS_SPACE_2), &
         arg_type(GH_OPERATOR, GH_REAL, GH_READWRITE, ANY_DISCONTINUOUS_SPACE_4,  &
                                                      ANY_DISCONTINUOUS_SPACE_1)  &
         /)
    type(func_type) :: meta_funcs(2) = (/                                         &
         func_type(ANY_DISCONTINUOUS_SPACE_1, GH_BASIS),                          &
         func_type(ANY_DISCONTINUOUS_SPACE_4, GH_BASIS, GH_DIFF_BASIS)            &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = gh_quadrature_XYoZ
  contains
    procedure, public, nopass :: testkern_any_discontinuous_space_op_2_code
  end type testkern_any_discontinuous_space_op_2_type

contains

  subroutine testkern_any_discontinuous_space_op_2_code(                      &
                                        cell, nlayers, field1,                &
                                        ncell_3d_op1, op1, ncell_3d_op2, op2, &
                                        ndf_adspc4, undf_adspc4, map_adspc4,  &
                                        basis_adspc4, diff_basis_adspc4,      &
                                        ndf_adspc1, basis_adspc1, ndf_adspc2, &
                                        np_xy, np_z, weights_xy, weights_z)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_adspc4
    integer(kind=i_def), intent(in) :: undf_adspc4, ndf_adspc1, ndf_adspc2
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d_op1
    integer(kind=i_def), intent(in) :: ncell_3d_op2
    integer(kind=i_def), intent(in) :: np_xy, np_z
    integer(kind=i_def), intent(in), dimension(ndf_adspc4) :: map_adspc4
    real(kind=r_def), intent(in), dimension(undf_adspc4) :: field1
    real(kind=r_def), intent(in), dimension(ndf_adspc1,ndf_adspc2,ncell_3d_op1)    :: op1
    real(kind=r_def), intent(inout), dimension(ndf_adspc4,ndf_adspc1,ncell_3d_op2) :: op2
    real(kind=r_def), intent(in), dimension(1,ndf_adspc1,np_xy,np_z) :: basis_adspc1
    real(kind=r_def), intent(in), dimension(1,ndf_adspc4,np_xy,np_z) :: basis_adspc4
    real(kind=r_def), intent(in), dimension(3,ndf_adspc4,np_xy,np_z) :: diff_basis_adspc4
    real(kind=r_def), intent(in), dimension(np_xy) :: weights_xy
    real(kind=r_def), intent(in), dimension(np_z)  :: weights_z

  end subroutine testkern_any_discontinuous_space_op_2_code

end module testkern_any_discontinuous_space_op_2_mod
