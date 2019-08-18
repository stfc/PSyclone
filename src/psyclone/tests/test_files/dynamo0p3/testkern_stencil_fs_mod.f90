! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2019, Science and Technology Facilities Council
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
! Author R. W. Ford, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

module testkern_stencil_fs_mod

  use constants_mod
  use argument_mod
  use kernel_mod

  implicit none

  ! Example of stencils on all supported function space identifiers
  type, extends(kernel_type) :: testkern_stencil_fs_type
     type(arg_type), dimension(12) :: meta_args = (/                &
          arg_type(gh_field, gh_inc,  w1),                          &
          arg_type(gh_field, gh_read, w0,          stencil(cross)), &
          arg_type(gh_field, gh_read, w1,          stencil(cross)), &
          arg_type(gh_field, gh_read, w2,          stencil(cross)), &
          arg_type(gh_field, gh_read, w3,          stencil(cross)), &
          arg_type(gh_field, gh_read, wtheta,      stencil(cross)), &
          arg_type(gh_field, gh_read, w2h,         stencil(cross)), &
          arg_type(gh_field, gh_read, w2v,         stencil(cross)), &
          arg_type(gh_field, gh_read, any_w2,      stencil(cross)), &
          arg_type(gh_field, gh_read, any_space_1, stencil(cross)), &
          arg_type(gh_field, gh_read, any_space_9, stencil(cross)), &
          arg_type(gh_field, gh_read, any_discontinuous_space_1,    &
                                      stencil(cross))               &
           /)
     integer :: iterates_over = cells
   contains
     procedure, public, nopass :: code => testkern_stencil_fs_code
  end type testkern_stencil_fs_type
contains

  subroutine testkern_stencil_fs_code(nlayers, field1,                       &
                      field2, field2_stencil_size, field2_stencil_dofmap,    &
                      field3, field3_stencil_size, field3_stencil_dofmap,    &
                      field4, field4_stencil_size, field4_stencil_dofmap,    &
                      field5, field5_stencil_size, field5_stencil_dofmap,    &
                      field6, field6_stencil_size, field6_stencil_dofmap,    &
                      field7, field7_stencil_size, field7_stencil_dofmap,    &
                      field8, field8_stencil_size, field8_stencil_dofmap,    &
                      field9, field9_stencil_size, field9_stencil_dofmap,    &
                      field10, field10_stencil_size, field10_stencil_dofmap, &
                      field11, field11_stencil_size, field11_stencil_dofmap, &
                      field12, field12_stencil_size, field12_stencil_dofmap, &
                      ndf_w1, undf_w1, map_w1,                               &
                      ndf_w0, undf_w0, map_w0,                               &
                      ndf_w2, undf_w2, map_w2,                               &
                      ndf_w3, undf_w3, map_w3,                               &
                      ndf_wtheta, undf_wtheta, map_wtheta,                   &
                      ndf_w2h, undf_w2h, map_w2h,                            &
                      ndf_w2v, undf_w2v, map_w2v,                            &
                      ndf_any_w2, undf_any_w2, map_any_w2,                   &
                      ndf_anyspace_1, undf_anyspace_1, map_anyspace_1,       &
                      ndf_anyspace_9, undf_anyspace_9, map_anyspace_9,       &
                      ndf_anydspace_1, undf_anydspace_1, map_anydspace_1)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in) :: ndf_w2h
    integer(kind=i_def), intent(in) :: ndf_w2v
    integer(kind=i_def), intent(in) :: ndf_any_w2
    integer(kind=i_def), intent(in) :: ndf_anyspace_1
    integer(kind=i_def), intent(in) :: ndf_anyspace_9
    integer(kind=i_def), intent(in) :: ndf_anydspace_1
    integer(kind=i_def), intent(in) :: undf_w1, undf_w0, undf_w2, undf_w3, &
                                       undf_wtheta, undf_w2h, undf_w2v,    &
                                       undf_any_w2, undf_anyspace_1,       &
                                       undf_anyspace_9, undf_anydspace_1
    integer(kind=i_def), intent(in) :: field2_stencil_size, field3_stencil_size,   &
                                       field4_stencil_size, field5_stencil_size,   &
                                       field6_stencil_size, field7_stencil_size,   &
                                       field8_stencil_size, field9_stencil_size,   &
                                       field10_stencil_size, field11_stencil_size, &
                                       field12_stencil_size
    integer(kind=i_def), intent(in), dimension(ndf_w1)          :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w0)          :: map_w0
    integer(kind=i_def), intent(in), dimension(ndf_w2)          :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w2h)         :: map_w2h
    integer(kind=i_def), intent(in), dimension(ndf_w2v)         :: map_w2v
    integer(kind=i_def), intent(in), dimension(ndf_w3)          :: map_w3
    integer(kind=i_def), intent(in), dimension(ndf_wtheta)      :: map_wtheta
    integer(kind=i_def), intent(in), dimension(ndf_any_w2)      :: map_any_w2
    integer(kind=i_def), intent(in), dimension(ndf_anydspace_1) :: map_anydspace_1
    integer(kind=i_def), intent(in), dimension(ndf_anyspace_1)  :: map_anyspace_1
    integer(kind=i_def), intent(in), dimension(ndf_anyspace_9)  :: map_anyspace_9
    integer(kind=i_def), intent(in), dimension(ndf_w0,field2_stencil_size)           :: field2_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w1,field3_stencil_size)           :: field3_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2,field4_stencil_size)           :: field4_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w3,field5_stencil_size)           :: field5_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_wtheta,field6_stencil_size)       :: field6_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2h,field7_stencil_size)          :: field7_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2v,field8_stencil_size)          :: field8_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_any_w2,field9_stencil_size)       :: field9_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_anyspace_1,field10_stencil_size)  :: field10_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_anyspace_9,field11_stencil_size)  :: field11_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_anydspace_1,field12_stencil_size) :: field12_stencil_dofmap
    real(kind=r_def), intent(inout), dimension(undf_w1)       :: field1
    real(kind=r_def), intent(in), dimension(undf_w0)          :: field2
    real(kind=r_def), intent(in), dimension(undf_w1)          :: field3
    real(kind=r_def), intent(in), dimension(undf_w2)          :: field4
    real(kind=r_def), intent(in), dimension(undf_w3)          :: field5
    real(kind=r_def), intent(in), dimension(undf_wtheta)      :: field6
    real(kind=r_def), intent(in), dimension(undf_w2h)         :: field7
    real(kind=r_def), intent(in), dimension(undf_w2v)         :: field8
    real(kind=r_def), intent(in), dimension(undf_any_w2)      :: field9
    real(kind=r_def), intent(in), dimension(undf_anyspace_1)  :: field10
    real(kind=r_def), intent(in), dimension(undf_anyspace_9)  :: field11
    real(kind=r_def), intent(in), dimension(undf_anydspace_1) :: field12

  end subroutine testkern_stencil_fs_code

end module testkern_stencil_fs_mod
