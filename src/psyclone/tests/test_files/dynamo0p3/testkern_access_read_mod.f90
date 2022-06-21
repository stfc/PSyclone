!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2022, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!C
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
! Modified: J. Henrichs, Bureau of Meteorology

!> @brief Kernel which assembles a locally assembled matrix (LMA) into a
!!        columnwise assembled matrix (CMA).
module testkern_access_read_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                 &
                                    GH_OPERATOR, GH_COLUMNWISE_OPERATOR, &
                                    GH_REAL, GH_READ, GH_WRITE,          &
                                    ANY_DISCONTINUOUS_SPACE_1,           &
                                    ANY_DISCONTINUOUS_SPACE_2,           &
                                    CELL_COLUMN

use constants_mod,           only : r_def, r_solver, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: testkern_access_read_type
  private
  type(arg_type) :: meta_args(3) = (/                                  &
       arg_type(GH_FIELD,               GH_REAL, GH_WRITE,             &
                ANY_DISCONTINUOUS_SPACE_1),                            &
       arg_type(GH_FIELD,               GH_REAL, GH_READ,              &
                ANY_DISCONTINUOUS_SPACE_2),                            &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ,              &
                ANY_DISCONTINUOUS_SPACE_1, ANY_DISCONTINUOUS_SPACE_2)  &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: testkern_access_read_code
end type testkern_access_read_type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public testkern_access_read_code

contains

  subroutine testkern_access_read_code(cell, ncell_2d, field_1_adspc1_field_1, &
      field_2_adspc2_field_2, cma_op_3, cma_op_3_nrow, cma_op_3_ncol,          &
      cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m,     &
      cma_op_3_gamma_p, ndf_adspc1_field_1, undf_adspc1_field_1,               &
      map_adspc1_field_1, cma_indirection_map_adspc1_field_1,                  &
      ndf_adspc2_field_2, undf_adspc2_field_2, map_adspc2_field_2,             &
      cma_indirection_map_adspc2_field_2)

      USE constants_mod
      IMPLICIT NONE

      INTEGER(KIND=i_def), intent(in) :: ndf_adspc1_field_1
      INTEGER(KIND=i_def), intent(in), dimension(ndf_adspc1_field_1) :: map_adspc1_field_1
      INTEGER(KIND=i_def), intent(in) :: ndf_adspc2_field_2
      INTEGER(KIND=i_def), intent(in), dimension(ndf_adspc2_field_2) :: map_adspc2_field_2
      INTEGER(KIND=i_def), intent(in) :: cma_op_3_nrow
      INTEGER(KIND=i_def), intent(in), dimension(cma_op_3_nrow) :: cma_indirection_map_adspc1_field_1
      INTEGER(KIND=i_def), intent(in) :: cma_op_3_ncol
      INTEGER(KIND=i_def), intent(in), dimension(cma_op_3_ncol) :: cma_indirection_map_adspc2_field_2
      INTEGER(KIND=i_def), intent(in) :: undf_adspc1_field_1, undf_adspc2_field_2
      INTEGER(KIND=i_def), intent(in) :: cell, ncell_2d
      INTEGER(KIND=i_def), intent(in) :: cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p
      REAL(KIND=r_solver), intent(in), dimension(cma_op_3_bandwidth,cma_op_3_nrow,ncell_2d) :: cma_op_3
      REAL(KIND=r_def), intent(inout), dimension(undf_adspc1_field_1) :: field_1_adspc1_field_1
      REAL(KIND=r_def), intent(in), dimension(undf_adspc2_field_2) :: field_2_adspc2_field_2

      write(*,*) "read mod"

    END SUBROUTINE testkern_access_read_code

end module testkern_access_read_mod
