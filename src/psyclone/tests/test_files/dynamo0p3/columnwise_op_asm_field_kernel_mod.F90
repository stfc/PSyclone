!-------------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-------------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications Copyright (c) 2017, Science and Technology Facilities Council
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
! Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

module columnwise_op_asm_field_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type, GH_FIELD,          &
                                    GH_OPERATOR, GH_COLUMNWISE_OPERATOR,    &
                                    GH_READ, GH_WRITE,                      &
                                    ANY_SPACE_1, ANY_SPACE_2,               &
                                    GH_COLUMN_BANDED_DOFMAP,                &
                                    CELLS 

use constants_mod,           only : r_def, i_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_asm_field_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                        &
       arg_type(GH_FIELD,               GH_READ,  ANY_SPACE_1),              &
       arg_type(GH_OPERATOR,            GH_READ,  ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_WRITE, ANY_SPACE_1, ANY_SPACE_2) &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: columnwise_op_asm_field_kernel_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface columnwise_op_asm_field_kernel_type
   module procedure columnwise_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_asm_field_kernel_code
contains
  
  type(columnwise_op_asm_field_kernel_type) function columnwise_constructor() result(self)
    implicit none
    return
  end function columnwise_constructor

  SUBROUTINE columnwise_op_asm_field_kernel_code(cell, nlayers, ncell_2d, &
       field_1_any_space_1_field_1, op_2_ncell_3d, op_2, cma_op_3, cma_op_3_nrow, &
       cma_op_3_ncol, cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, &
       cma_op_3_gamma_m, cma_op_3_gamma_p, ndf_any_space_1_field_1, &
       undf_any_space_1_field_1, map_any_space_1_field_1, &
       cbanded_map_any_space_1_field_1, ndf_any_space_2_op_2, &
       cbanded_map_any_space_2_op_2)
      USE constants_mod, ONLY: r_def
      IMPLICIT NONE
      INTEGER, intent(in) :: cell
      INTEGER, intent(in) :: nlayers
      INTEGER, intent(in) :: ncell_2d
      INTEGER, intent(in) :: ndf_any_space_1_field_1
      INTEGER, intent(in) :: undf_any_space_1_field_1
      INTEGER, intent(in) :: ndf_any_space_2_op_2
      REAL(KIND=r_def), intent(in), &
           dimension(undf_any_space_1_field_1) :: field_1_any_space_1_field_1
      INTEGER, intent(in) :: op_2_ncell_3d
      REAL(KIND=r_def), intent(in), dimension(ndf_any_space_1_field_1, &
           ndf_any_space_2_op_2,op_2_ncell_3d) :: op_2
      INTEGER, intent(in) :: cma_op_3_nrow, cma_op_3_ncol, cma_op_3_bandwidth, &
           cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p
      REAL(KIND=r_def), intent(out), dimension(cma_op_3_bandwidth, &
           cma_op_3_nrow,ncell_2d) :: cma_op_3
      INTEGER, intent(in), dimension(ndf_any_space_1_field_1) :: &
           map_any_space_1_field_1
      INTEGER, intent(in), dimension(ndf_any_space_1_field_1,nlayers) :: &
           cbanded_map_any_space_1_field_1
      INTEGER, intent(in), dimension(ndf_any_space_2_op_2,nlayers) :: &
           cbanded_map_any_space_2_op_2

    write (*,*) "Hello CMA World"

  end subroutine columnwise_op_asm_field_kernel_code

end module columnwise_op_asm_field_kernel_mod
