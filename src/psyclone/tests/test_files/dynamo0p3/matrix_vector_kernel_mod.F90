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
! Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

module matrix_vector_kernel_mod


use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,      &
                                    GH_FIELD, GH_OPERATOR,    &
                                    GH_REAL, GH_READ, GH_INC, &
                                    ANY_SPACE_1, CELL_COLUMN
use constants_mod,           only : r_def, i_def

implicit none

private

type, public, extends(kernel_type) :: matrix_vector_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                    &
       arg_type(GH_FIELD,    GH_REAL, GH_INC,  ANY_SPACE_1),             &
       arg_type(GH_FIELD,    GH_REAL, GH_READ, ANY_SPACE_1),             &
       arg_type(GH_OPERATOR, GH_REAL, GH_READ, ANY_SPACE_1, ANY_SPACE_1) &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: matrix_vector_code
end type

public matrix_vector_code

contains

  subroutine matrix_vector_code(cell, nlayers,  &
                                field1, field2, &
                                ncell_3d, op_3, &
                                ndf1, undf1, map1)

    implicit none

    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf1
    integer(kind=i_def), intent(in) :: undf1
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in), dimension(ndf1) :: map1
    real(kind=r_def), intent(inout), dimension(undf1) :: field1
    real(kind=r_def), intent(in), dimension(undf1)    :: field2
    real(kind=r_def), intent(in), dimension(ndf1,ndf1,ncell_3d) :: op_3

  end subroutine matrix_vector_code

end module matrix_vector_kernel_mod
