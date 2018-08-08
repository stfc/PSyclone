!-------------------------------------------------------------------------------
! Copyright (c) 2017, Science and Technology Facilities Council
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
!
! Author: A. R. Porter STFC Daresbury Lab
!-------------------------------------------------------------------------------

module testkern_eval_anyspace2
  use argument_mod
  use kernel_mod
  type, extends(kernel_type) :: testkern_eval_anyspace2_type
     type(arg_type)  :: meta_args(3) =  (/    &
       arg_type(GH_FIELD,   GH_WRITE,  ANY_SPACE_1),   &
       arg_type(GH_FIELD,   GH_READ, W0),              &
       arg_type(GH_FIELD,   GH_READ, W1)               &
       /)
     type(func_type) :: meta_funcs(2) = (/                           &
       func_type(W0, GH_BASIS),                                      &
       func_type(W1, GH_DIFF_BASIS)                                  &
       /)
     integer :: iterates_over = cells
     integer :: gh_shape = gh_evaluator
   contains
     procedure, nopass :: code => testkern_eval_anyspace2_code
  end type testkern_eval_anyspace2_type
contains

  subroutine testkern_eval_anyspace2_code(nlayers, f0, f1, f2,               &
                ndf_any_space_1_f0, undf_any_space_1_f0, map_any_space_1_f0, &
                ndf_w0, undf_w0, map_w0, basis_w0_on_any_space_1_f0,         &
                ndf_w1, undf_w1, map_w1, diff_basis_w1_on_any_space_1_f0)
    use constants_mod, only: r_def
    implicit none
    integer :: nlayers, ndf_any_space_1_f0, undf_any_space_1_f0, ndf_w0, &
               undf_w0, ndf_w1, undf_w1
    integer, dimension(:) :: map_any_space_1_f0, map_w0, map_w1
    real(kind=r_def), dimension(:) :: f0, f1, f2
    real(kind=r_def), dimension(:,:,:) :: basis_w0_on_any_space_1_f0, &
                                          diff_basis_w1_on_any_space_1_f0
  end subroutine testkern_eval_anyspace2_code
end module testkern_eval_anyspace2
