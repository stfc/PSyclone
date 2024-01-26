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
! Author R. W. Ford STFC Daresbury Lab
! Modified I. Kavcic Met Office

module testkern_anyw2_stencil_mod

  use argument_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_anyw2_stencil_type
     type(arg_type), dimension(3) :: meta_args =                           &
          (/ arg_type(gh_field, gh_real, gh_inc,  any_w2),                 &
             arg_type(gh_field, gh_real, gh_read, any_w2, stencil(cross)), &
             arg_type(gh_field, gh_real, gh_read, any_w2, stencil(cross))  &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_anyw2_stencil_code
  end type testkern_anyw2_stencil_type

contains

  subroutine testkern_anyw2_stencil_code(nlayers, fld1,                 &
                                    fld2, fld2_st_size, fld2_st_dofmap, &
                                    fld3, fld3_st_size, fld3_st_dofmap, &
                                    ndf_any_w2, undf_any_w2, map_any_w2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_any_w2
    integer(kind=i_def), intent(in) :: undf_any_w2
    integer(kind=i_def), intent(in) :: fld2_st_size, fld3_st_size
    integer(kind=i_def), intent(in), dimension(ndf_any_w2) :: map_any_w2
    integer(kind=i_def), intent(in), dimension(ndf_any_w2,fld2_st_size) :: fld2_st_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_any_w2,fld3_st_size) :: fld3_st_dofmap
    real(kind=r_def), intent(inout), dimension(undf_any_w2) :: fld1
    real(kind=r_def), intent(in), dimension(undf_any_w2) :: fld2
    real(kind=r_def), intent(in), dimension(undf_any_w2) :: fld3

  end subroutine testkern_anyw2_stencil_code

end module testkern_anyw2_stencil_mod
