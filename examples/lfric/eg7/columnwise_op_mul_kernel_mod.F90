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
! Modifications copyright (c) 2017-2020, Science and Technology Facilities Council
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
! Modified by I. Kavcic, Met Office
!
!> @brief Kernel which calculates the product of two columnwise operators
!> @details Calculates op_C = op_C + op_A * op_B
module columnwise_op_mul_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type,                               &
                                    GH_COLUMNWISE_OPERATOR,                 &
                                    GH_READ, GH_WRITE, GH_INC,              &
                                    ANY_SPACE_1, ANY_SPACE_2, ANY_SPACE_3,  &
                                    CELLS

use constants_mod,           only : r_def, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_mul_kernel_type
   private
  type(arg_type) :: meta_args(3) = (/                                             &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ,      ANY_SPACE_1, ANY_SPACE_2),  &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ,      ANY_SPACE_2, ANY_SPACE_3),  &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READWRITE, ANY_SPACE_1, ANY_SPACE_3)   &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: columnwise_op_mul_kernel_code
end type columnwise_op_mul_kernel_type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_mul_kernel_code

contains

  !> @brief The subroutine which is called directly from the PSY layer and
  !> calculates op_C = op_C + op_A * op_B
  !>
  !> @param [in] cell the horizontal cell index
  !> @param [in] ncell_2d total number of cells in 2d grid
  !> @param [in] columnwise_matrix_A banded matrix op_A
  !> @param [in] nrow_A number of rows in the banded matrix A
  !> @param [in] ncol_A number of columns in the banded matrix A
  !> @param [in] bandwidth_A bandwidth of the banded matrix
  !> @param [in] alpha_A banded matrix parameter \f$\alpha\f$
  !> @param [in] beta_A banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m_A banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p_A banded matrix parameter \f$\gamma_+\f$
  !> @param [in] columnwise_matrix_B banded matrix op_B
  !> @param [in] nrow_B number of rows in the banded matrix B
  !> @param [in] ncol_B number of columns in the banded matrix B
  !> @param [in] bandwidth_B bandwidth of the banded matrix
  !> @param [in] alpha_B banded matrix parameter \f$\alpha\f$
  !> @param [in] beta_B banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m_B banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p_B banded matrix parameter \f$\gamma_+\f$
  !> @param [in,out] columnwise_matrix_C banded matrix op_C
  !> @param [in] nrow_C number of rows in the banded matrix C
  !> @param [in] ncol_C number of columns in the banded matrix C
  !> @param [in] bandwidth_C bandwidth of the banded matrix
  !> @param [in] alpha_C banded matrix parameter \f$\alpha\f$
  !> @param [in] beta_C banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m_C banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p_C banded matrix parameter \f$\gamma_+\f$
  subroutine columnwise_op_mul_kernel_code(cell,                      &
                                           ncell_2d,                  &
                                           columnwise_matrix_A,       &
                                           nrow_A, ncol_A,            &
                                           bandwidth_A,               &
                                           alpha_A,                   &
                                           beta_A,                    &
                                           gamma_m_A,                 &
                                           gamma_p_A,                 &
                                           columnwise_matrix_B,       &
                                           nrow_B, ncol_B,            &
                                           bandwidth_B,               &
                                           alpha_B,                   &
                                           beta_B,                    &
                                           gamma_m_B,                 &
                                           gamma_p_B,                 &
                                           columnwise_matrix_C,       &
                                           nrow_C, ncol_C,            &
                                           bandwidth_C,               &
                                           alpha_C,                   &
                                           beta_C,                    &
                                           gamma_m_C,                 &
                                           gamma_p_C)

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: cell,  ncell_2d
    integer(kind=i_def), intent(in) :: nrow_A, ncol_A
    integer(kind=i_def), intent(in) :: nrow_B, ncol_B
    integer(kind=i_def), intent(in) :: nrow_C, ncol_C
    integer(kind=i_def), intent(in) :: bandwidth_A, bandwidth_B, bandwidth_C
    real(kind=r_def), dimension(bandwidth_A,nrow_A,ncell_2d), intent(in) :: columnwise_matrix_A
    real(kind=r_def), dimension(bandwidth_B,nrow_B,ncell_2d), intent(in) :: columnwise_matrix_B
    real(kind=r_def), dimension(bandwidth_C,nrow_C,ncell_2d), intent(out) :: columnwise_matrix_C

    integer(kind=i_def), intent(in) :: alpha_A, beta_A, gamma_m_A, gamma_p_A
    integer(kind=i_def), intent(in) :: alpha_B, beta_B, gamma_m_B, gamma_p_B
    integer(kind=i_def), intent(in) :: alpha_C, beta_C, gamma_m_C, gamma_p_C

    ! Internal parameters
    integer(kind=i_def) :: i,j,ell ! Row and column index index
    ! Smallest index in a particular row
    integer(kind=i_def) :: j_minus_A, j_minus_B, j_minus_C
    integer(kind=i_def) :: j_plus_A, j_plus_B, j_plus_C
    real(kind=r_def) :: matrixentry

    do i=1, nrow_C
       j_minus_A = ceiling((alpha_A*i-gamma_p_A)/(1.0_r_def*beta_A),i_def)
       j_plus_A = floor((alpha_A*i+gamma_m_A)/(1.0_r_def*beta_A),i_def)
       j_minus_C = ceiling((alpha_C*i-gamma_p_C)/(1.0_r_def*beta_C),i_def)
       j_plus_C = floor((alpha_C*i+gamma_m_C)/(1.0_r_def*beta_C),i_def)
       do j=MAX(1,j_minus_C), MIN(ncol_C,j_plus_C)
          matrixentry = 0.0_r_def
          do ell=MAX(1,j_minus_A), MIN(ncol_A,j_plus_A)
             j_minus_B = ceiling((alpha_B*ell-gamma_p_B)/(1.0_r_def*beta_B),i_def)
             j_plus_B = floor((alpha_B*ell+gamma_m_B)/(1.0_r_def*beta_B),i_def)
             if ( (j_minus_B <= j) .and. (j <= j_plus_B) ) then
                matrixentry = matrixentry                               &
                            + columnwise_matrix_A(ell-j_minus_A+1,i,cell) &
                            * columnwise_matrix_B(j-j_minus_B+1,ell,cell)
             end if
         end do
         columnwise_matrix_C(j-j_minus_C+1,i,cell) &
           = columnwise_matrix_C(j-j_minus_C+1,i,cell) + matrixentry
       end do
    end do

  end subroutine columnwise_op_mul_kernel_code

end module columnwise_op_mul_kernel_mod
