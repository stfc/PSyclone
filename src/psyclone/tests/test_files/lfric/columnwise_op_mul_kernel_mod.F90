! -----------------------------------------------------------------------------
! Original under:
! Copyright (c) 2017-2026,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! Modifications under:
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

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
