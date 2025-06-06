!-----------------------------------------------------------------------------
! Copyright (c) 2017-2025,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2025, Science and Technology Facilities Council
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
! Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
! Modified: I. Kavcic, Met Office

!> @brief Kernel which applies a columnwise assembled operator to a field on
!!        any discontinuous space.
module columnwise_op_app_anydspace_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                    &
                                    GH_FIELD, GH_COLUMNWISE_OPERATOR,       &
                                    GH_REAL, GH_READ, GH_WRITE,             &
                                    ANY_DISCONTINUOUS_SPACE_1, ANY_SPACE_1, &
                                    CELL_COLUMN

use constants_mod,           only : r_def, r_solver, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_app_anydspace_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                                  &
       arg_type(GH_FIELD,               GH_REAL, GH_WRITE, ANY_DISCONTINUOUS_SPACE_1), &
       arg_type(GH_FIELD,               GH_REAL, GH_READ,  ANY_SPACE_1),               &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ,  ANY_DISCONTINUOUS_SPACE_1,  &
                                                           ANY_SPACE_1)                &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: columnwise_op_app_anydspace_kernel_code
end type columnwise_op_app_anydspace_kernel_type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_app_anydspace_kernel_code

contains

  subroutine columnwise_op_app_anydspace_kernel_code(      &
                               cell,                       &
                               ncell_2d,                   &
                               field1,                     &
                               field2,                     &
                               cma_op, cma_op_nrow,        &
                               cma_op_ncol,                &
                               cma_op_bandwidth,           &
                               cma_op_alpha,               &
                               cma_op_beta,                &
                               cma_op_gamma_m,             &
                               cma_op_gamma_p,             &
                               ndf_adspc1,                 &
                               undf_adspc1,                &
                               map_adspc1,                 &
                               cma_indirection_map_adspc1, &
                               ndf_aspc1,                  &
                               undf_aspc1,                 &
                               map_aspc1,                  &
                               cma_indirection_map_aspc1)

    implicit none

    integer(kind=i_def), intent(in) :: cell, ncell_2d
    integer(kind=i_def), intent(in) :: ndf_adspc1
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: undf_adspc1
    integer(kind=i_def), intent(in) :: undf_aspc1
    integer(kind=i_def), intent(in) :: cma_op_nrow
    integer(kind=i_def), intent(in) :: cma_op_ncol
    integer(kind=i_def), intent(in) :: cma_op_bandwidth
    integer(kind=i_def), intent(in) :: cma_op_alpha, cma_op_beta
    integer(kind=i_def), intent(in) :: cma_op_gamma_m, cma_op_gamma_p
    integer(kind=i_def), intent(in), dimension(ndf_adspc1)  :: map_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_aspc1)   :: map_aspc1
    integer(kind=i_def), intent(in), dimension(cma_op_nrow) :: &
                                     cma_indirection_map_adspc1
    integer(kind=i_def), intent(in), dimension(cma_op_ncol) :: &
                                     cma_indirection_map_aspc1
    real(kind=r_def), intent(inout), dimension(undf_adspc1) :: field1
    real(kind=r_def), intent(in), dimension(undf_aspc1)     :: field2
    real(kind=r_solver), intent(in), dimension(cma_op_bandwidth,cma_op_nrow,ncell_2d) :: cma_op

    write(*,*) "A kernel that applies CMA operator to a field on &
                discontinuous space ANY_DISCONTINUOUS_SPACE_1"

  end subroutine columnwise_op_app_anydspace_kernel_code

end module columnwise_op_app_anydspace_kernel_mod
