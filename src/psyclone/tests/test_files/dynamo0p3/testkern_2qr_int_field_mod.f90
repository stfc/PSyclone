! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2021-2024, Science and Technology Facilities Council
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
! Author I. Kavcic Met Office

module testkern_2qr_int_field_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  ! Integer-valued fields on continuous (w2), read-only (wchi) and
  ! discontinuous (any_discontinuous_space_1) function spaces with two
  ! quadrature shapes and basis/differential basis functions on each space
  type, extends(kernel_type) :: testkern_2qr_int_field_type
     type(arg_type), dimension(4) :: meta_args =              &
          (/ arg_type(gh_field,   gh_integer, gh_inc,  w2),   &
             arg_type(gh_field*3, gh_integer, gh_read, wchi), &
             arg_type(gh_field,   gh_integer, gh_read,        &
                                  any_discontinuous_space_1), &
             arg_type(gh_scalar,  gh_integer, gh_read)        &
           /)
     type(func_type), dimension(3) :: meta_funcs =            &
          (/ func_type(w2,   gh_basis),                       &
             func_type(wchi, gh_diff_basis),                  &
             func_type(any_discontinuous_space_1, gh_basis,   &
                                               gh_diff_basis) &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape(2) = (/ gh_quadrature_XYoZ, gh_quadrature_face /)
   contains
     procedure, nopass :: code => testkern_2qr_int_field_code
  end type testkern_2qr_int_field_type

contains

  subroutine testkern_2qr_int_field_code(nlayers, field1,            &
                              field2_v1, field2_v2, field2_v3,       &
                              field3, iscalar,                       &
                              ndf_w2, undf_w2, map_w2,               &
                              basis_w2_qr_xyoz, basis_w2_qr_face,    &
                              ndf_wchi, undf_wchi, map_wchi,         &
                              diff_basis_wchi_qr_xyoz,               &
                              diff_basis_wchi_qr_face,               &
                              ndf_adspc1, undf_adspc1, map_adspc1,   &
                              basis_adspc1_qr_xyoz,                  &
                              basis_adspc1_qr_face,                  &
                              diff_basis_adspc1_qr_xyoz,             &
                              diff_basis_adspc1_qr_face,             &
                              np_xy_qr_xyoz, np_z_qr_xyoz,           &
                              weights_xy_qr_xyoz, weights_z_qr_xyoz, &
                              nfaces_qr_face, np_xyz_qr_face, weights_xyz_qr_face)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_adspc1
    integer(kind=i_def), intent(in) :: ndf_wchi
    integer(kind=i_def), intent(in) :: undf_w2, undf_wchi, undf_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_w2)     :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_adspc1) :: map_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_wchi)   :: map_wchi
    integer(kind=i_def), intent(in) :: iscalar
    integer(kind=i_def), intent(inout), dimension(undf_w2)  :: field1
    integer(kind=i_def), intent(in), dimension(undf_wchi)   :: field2_v1
    integer(kind=i_def), intent(in), dimension(undf_wchi)   :: field2_v2
    integer(kind=i_def), intent(in), dimension(undf_wchi)   :: field2_v3
    integer(kind=i_def), intent(in), dimension(undf_adspc1) :: field3
    integer(kind=i_def), intent(in) :: np_xy_qr_xyoz, np_z_qr_xyoz, np_xyz_qr_face, nfaces_qr_face
    real(kind=r_def), intent(in), dimension(3,ndf_w2,np_xy_qr_xyoz,np_z_qr_xyoz)        :: basis_w2_qr_xyoz
    real(kind=r_def), intent(in), dimension(3,ndf_w2,np_xyz_qr_face,nfaces_qr_face)     :: basis_w2_qr_face
    real(kind=r_def), intent(in), dimension(3,ndf_wchi,np_xy_qr_xyoz,np_z_qr_xyoz)      :: diff_basis_wchi_qr_xyoz
    real(kind=r_def), intent(in), dimension(3,ndf_wchi,np_xyz_qr_face,nfaces_qr_face)   :: diff_basis_wchi_qr_face
    real(kind=r_def), intent(in), dimension(1,ndf_adspc1,np_xy_qr_xyoz,np_z_qr_xyoz)    :: basis_adspc1_qr_xyoz
    real(kind=r_def), intent(in), dimension(3,ndf_adspc1,np_xy_qr_xyoz,np_z_qr_xyoz)    :: diff_basis_adspc1_qr_xyoz
    real(kind=r_def), intent(in), dimension(1,ndf_adspc1,np_xyz_qr_face,nfaces_qr_face) :: basis_adspc1_qr_face
    real(kind=r_def), intent(in), dimension(3,ndf_adspc1,np_xyz_qr_face,nfaces_qr_face) :: diff_basis_adspc1_qr_face
    real(kind=r_def), intent(in), dimension(np_xy_qr_xyoz)                 :: weights_xy_qr_xyoz
    real(kind=r_def), intent(in), dimension(np_z_qr_xyoz)                  :: weights_z_qr_xyoz
    real(kind=r_def), intent(in), dimension(np_xyz_qr_face,nfaces_qr_face) :: weights_xyz_qr_face

  end subroutine testkern_2qr_int_field_code

end module testkern_2qr_int_field_mod
