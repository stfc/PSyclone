!-------------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-------------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2018, Science and Technology Facilities Council
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
! Modified I. Kavcic Met Office

! Kernel which applies a columnwise assembled operator to a field on W3 (discontinuous)
module columnwise_op_app_w3_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,              &
                                    GH_FIELD, GH_COLUMNWISE_OPERATOR, &
                                    GH_READ, GH_WRITE,                &
                                    W3, ANY_SPACE_1,                  &
                                    GH_COLUMN_INDIRECTION_DOFMAP,     &
                                    CELLS 

use constants_mod,           only : r_def, i_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_app_w3_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                              &
       arg_type(GH_FIELD,               GH_WRITE, W3),             &  
       arg_type(GH_FIELD,               GH_READ,  ANY_SPACE_1),    &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ,  W3, ANY_SPACE_1) &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: columnwise_op_app_w3_kernel_code
end type columnwise_op_app_w3_kernel_type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! Overload the default structure constructor for function space
interface columnwise_op_app_w3_kernel_type
   module procedure columnwise_op_app_w3_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_app_w3_kernel_code

contains
  
  type(columnwise_op_app_w3_kernel_type) function columnwise_op_app_w3_kernel_constructor() result(self)
    implicit none
    return
  end function columnwise_op_app_w3_kernel_constructor

  SUBROUTINE columnwise_op_app_w3_kernel_code(cell,                        &
                                              ncell_2d,                    &
                                              field_1_w3,                  &
                                              field_2_any_space_1_field_2, &
                                              cma_op_3,                    &
                                              cma_op_3_nrow,               &
                                              cma_op_3_ncol,               &
                                              cma_op_3_bandwidth,          &
                                              cma_op_3_alpha,              &
                                              cma_op_3_beta,               &
                                              cma_op_3_gamma_m,            &
                                              cma_op_3_gamma_p,            &
                                              ndf_w3, undf_w3, map_w3,     &
                                              cma_indirection_map_w3,      &
                                              ndf_any_space_1_field_2,     &
                                              undf_any_space_1_field_2,    &
                                              map_any_space_1_field_2,     &
                                              cma_indirection_map_any_space_1_field_2)

    USE constants_mod, ONLY: r_def

    IMPLICIT NONE

    INTEGER, intent(in) :: cell
    INTEGER, intent(in) :: ncell_2d
    INTEGER, intent(in) :: ndf_w3
    INTEGER, intent(in) :: undf_w3
    INTEGER, intent(in) :: ndf_any_space_1_field_2
    INTEGER, intent(in) :: undf_any_space_1_field_2
    REAL(KIND=r_def), intent(out), dimension(undf_w3) :: field_1_w3
    REAL(KIND=r_def), intent(in), dimension(undf_any_space_1_field_2) :: field_2_any_space_1_field_2
    INTEGER, intent(in) :: cma_op_3_nrow, cma_op_3_ncol, cma_op_3_bandwidth
    INTEGER, intent(in) :: cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p
    REAL(KIND=r_def), intent(in), dimension(cma_op_3_bandwidth,cma_op_3_nrow,ncell_2d) :: cma_op_3
    INTEGER, intent(in), dimension(ndf_w3) :: map_w3
    INTEGER, intent(in), dimension(cma_op_3_nrow) :: cma_indirection_map_w3
    INTEGER, intent(in), dimension(ndf_any_space_1_field_2) :: map_any_space_1_field_2
    INTEGER, intent(in), dimension(cma_op_3_ncol) :: cma_indirection_map_any_space_1_field_2

    write (*,*) "A kernel that applies CMA operator to a field on discontinuous space W3"

  END SUBROUTINE columnwise_op_app_w3_kernel_code

end module columnwise_op_app_w3_kernel_mod
