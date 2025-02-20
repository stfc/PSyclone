! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
! Author R. W. Ford, STFC Daresbury Lab
! Modified I. Kavcic, O. Brunt, and A. Pirrie, Met Office

module testkern_dofs_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_dofs_type
     type(arg_type), dimension(6) :: meta_args =          &
          (/ arg_type(gh_field,   gh_real, gh_write, w1), &
             arg_type(gh_field,   gh_real, gh_read,  w1), &
             arg_type(gh_field,   gh_real, gh_read,  w1), &
             arg_type(gh_field,   gh_real, gh_read,  w1), &
             arg_type(gh_field*3, gh_real, gh_read,  w1), &
             arg_type(gh_scalar,  gh_real, gh_read)       &
           /)

     integer :: operates_on = DOF
   contains
     procedure, nopass :: code => testkern_dofs_code
  end type testkern_dofs_type

contains

  subroutine testkern_dofs_code(a, b, c, d,  &
                                field_vec_1, &
                                field_vec_2, &
                                field_vec_3, &
                                scalar_arg)
    implicit none

    real(kind=r_def), intent(inout) :: a
    real(kind=r_def), intent(in)    :: b, c, d
    real(kind=r_def), intent(in)    :: field_vec_1, field_vec_2, field_vec_3
    real(kind=r_def), intent(in)    :: scalar_arg

  end subroutine testkern_dofs_code

end module testkern_dofs_mod
