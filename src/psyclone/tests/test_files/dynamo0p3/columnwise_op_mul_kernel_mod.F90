!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2022, Science and Technology Facilities Council
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
!------------------------------------------------------------------------------
! Modified: I. Kavcic, Met Office

!> @brief Kernel which calculates the product of two columnwise operators,
!!        op_C = op_C + op_A * op_B.

module columnwise_op_mul_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type,                               &
                                    GH_COLUMNWISE_OPERATOR, GH_REAL,        &
                                    GH_READ, GH_WRITE, GH_READWRITE,        &
                                    ANY_SPACE_1, ANY_SPACE_2, ANY_SPACE_3,  &
                                    CELL_COLUMN

use constants_mod,           only : r_solver, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_mul_kernel_type
   private
  type(arg_type) :: meta_args(3) = (/                                                     &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ,      ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ,      ANY_SPACE_2, ANY_SPACE_3), &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READWRITE, ANY_SPACE_1, ANY_SPACE_3)  &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: columnwise_op_mul_kernel_code
end type columnwise_op_mul_kernel_type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_mul_kernel_code

contains

  !> @brief The subroutine which is called directly from the PSy layer and
  !!        calculates op_C = op_C + op_A * op_B.
  !>
  !> @param [in] cell Horizontal cell index
  !> @param [in] ncell_2d Total number of cells in 2d grid
  !> @param [in] columnwise_matrix_A Banded matrix op_A
  !> @param [in] nrow_A Number of rows in the banded matrix A
  !> @param [in] ncol_A Number of columns in the banded matrix A
  !> @param [in] bandwidth_A Bandwidth of the banded matrix
  !> @param [in] alpha_A Banded matrix parameter \f$\alpha\f$
  !> @param [in] beta_A Banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m_A Banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p_A Banded matrix parameter \f$\gamma_+\f$
  !> @param [in] columnwise_matrix_B Banded matrix op_B
  !> @param [in] nrow_B Number of rows in the banded matrix B
  !> @param [in] ncol_B Number of columns in the banded matrix B
  !> @param [in] bandwidth_B Bandwidth of the banded matrix
  !> @param [in] alpha_B Banded matrix parameter \f$\alpha\f$
  !> @param [in] beta_B Banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m_B Banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p_B Banded matrix parameter \f$\gamma_+\f$
  !> @param [in,out] columnwise_matrix_C Banded matrix op_C
  !> @param [in] nrow_C Number of rows in the banded matrix C
  !> @param [in] ncol_C Number of columns in the banded matrix C
  !> @param [in] bandwidth_C Bandwidth of the banded matrix
  !> @param [in] alpha_C Banded matrix parameter \f$\alpha\f$
  !> @param [in] beta_C Banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m_C Banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p_C Banded matrix parameter \f$\gamma_+\f$
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
    integer(kind=i_def), intent(in) :: alpha_A, beta_A, gamma_m_A, gamma_p_A
    integer(kind=i_def), intent(in) :: alpha_B, beta_B, gamma_m_B, gamma_p_B
    integer(kind=i_def), intent(in) :: alpha_C, beta_C, gamma_m_C, gamma_p_C
    real(kind=r_solver), dimension(bandwidth_A,nrow_A,ncell_2d), intent(in)    :: columnwise_matrix_A
    real(kind=r_solver), dimension(bandwidth_B,nrow_B,ncell_2d), intent(in)    :: columnwise_matrix_B
    real(kind=r_solver), dimension(bandwidth_C,nrow_C,ncell_2d), intent(inout) :: columnwise_matrix_C

  end subroutine columnwise_op_mul_kernel_code

end module columnwise_op_mul_kernel_mod
