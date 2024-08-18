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
! Author R. W. Ford STFC Daresbury Lab
! Modified I. Kavcic Met Office

module testkern_anyw2_vector_mod

  use argument_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_anyw2_vector_type
     type(arg_type), dimension(3) :: meta_args =             &
          (/ arg_type(gh_field,   gh_real, gh_inc,  any_w2), &
             arg_type(gh_field,   gh_real, gh_read, any_w2), &
             arg_type(gh_field*2, gh_real, gh_read, any_w2)  &
           /)
     type(func_type), dimension(1) :: meta_funcs =           &
          (/ func_type(any_w2, gh_basis, gh_diff_basis)      &
          /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_anyw2_vector_code
  end type testkern_anyw2_vector_type

contains

  subroutine testkern_anyw2_vector_code(nlayers, fld1, fld2,     &
                            fld3_v1, fld3_v2,                    &
                            ndf_any_w2, undf_any_w2, map_any_w2, &
                            basis_any_w2, diff_basis_any_w2,     &
                            np_xy, np_z, weights_xy, weights_z)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_any_w2
    integer(kind=i_def), intent(in) :: undf_any_w2
    integer(kind=i_def), intent(in) :: np_xy, np_z
    integer(kind=i_def), intent(in), dimension(ndf_any_w2) :: map_any_w2
    real(kind=r_def), intent(inout), dimension(undf_any_w2) :: fld1
    real(kind=r_def), intent(in), dimension(undf_any_w2)    :: fld2
    real(kind=r_def), intent(in), dimension(undf_any_w2)    :: fld3_v1
    real(kind=r_def), intent(in), dimension(undf_any_w2)    :: fld3_v2
    real(kind=r_def), intent(in), dimension(3,ndf_any_w2,np_xy,np_z) :: basis_any_w2
    real(kind=r_def), intent(in), dimension(1,ndf_any_w2,np_xy,np_z) :: diff_basis_any_w2
    real(kind=r_def), intent(in), dimension(np_xy) :: weights_xy
    real(kind=r_def), intent(in), dimension(np_z)  :: weights_z

  end subroutine testkern_anyw2_vector_code

end module testkern_anyw2_vector_mod
