! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2025, Science and Technology Facilities Council.
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
! Author A. Pirrie, Met Office

module testkern_scalar_array_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_scalar_array_type
     type(arg_type), dimension(4) :: meta_args =                  &
          (/ arg_type(gh_field,        gh_real,    gh_inc,  w1),  &
             arg_type(gh_scalar_array, gh_real,    gh_read, 2 ),  &
             arg_type(gh_scalar_array, gh_logical, gh_read, 1 ),  &
             arg_type(gh_scalar_array, gh_integer, gh_read, 4 )   &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_scalar_array_code
  end type testkern_scalar_array_type

contains

  subroutine testkern_scalar_array_code(nlayers,                    &
                                        afield,                     &
                                        dims_rarray, real_array,    &
                                        dims_larray, logical_array, &
                                        dims_iarray, integer_array, &
                                        )
    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    real(kind=r_def),    intent(inout) :: afield
    integer(kind=i_def), intent(in), dimension(2) :: dims_rarray
    integer(kind=i_def), intent(in), dimension(1) :: dims_larray
    integer(kind=i_def), intent(in), dimension(4) :: dims_iarray
    real(kind=r_def),    intent(in), dimension(dims_rarray(1),dims_rarray(2)) :: real_array
    logical(kind=l_def), intent(in), dimension(dims_larray(1)) :: logical_array
    integer(kind=i_def), intent(in), dimension(dims_iarray(1),dims_iarray(2),dims_iarray(3),dims_iarray(4)) :: integer_array

  end subroutine testkern_scalar_array_code

end module testkern_scalar_array_mod
