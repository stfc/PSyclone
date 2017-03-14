! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017, Science and Technology Facilities Council
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2016.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
! Author R. Ford and A. R. Porter, STFC Daresbury Lab

!> @brief Kernel which calculates the product of two columnwise operators
!> @detailled calculates op_C = op_C + op_A * op_B

module columnwise_op_mul_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type,                               &
                                    GH_COLUMNWISE_OPERATOR,                 &
                                    GH_READ, GH_WRITE, GH_INC,              &
                                    ANY_SPACE_1, ANY_SPACE_2, ANY_SPACE_3,  &
                                    CELLS 

use constants_mod,           only : r_def, i_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_mul_kernel_type
   private
  type(arg_type) :: meta_args(3) = (/                                        &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2),  &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_2, ANY_SPACE_3),  &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_INC, ANY_SPACE_1, ANY_SPACE_3)    &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: columnwise_op_mul_kernel_code
end type columnwise_op_mul_kernel_type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface columnwise_op_mul_kernel_type
   module procedure columnwise_op_mul_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_mul_kernel_code
contains
  
  type(columnwise_op_mul_kernel_type) function columnwise_op_mul_kernel_constructor() result(self)
    implicit none
    return
  end function columnwise_op_mul_kernel_constructor

  !> @brief The subroutine which is called directly from the PSY layer and
  !> calculates op_C = op_C + op_A * op_B 
  !>
  !> @param [in] cell the horizontal cell index
  !> @param [in] ncell_2d total number of cells in 2d grid
  !> @param [in] columnwise_matrix banded matrix op_A
  !> @param [in] columnwise_matrix banded matrix op_B
  !> @param [inout] columnwise_matrix banded matrix op_C
  !> @param [in] nrow_A number of rows in the banded matrix A
  !> @param [in] ncol_A number of columns in the banded matrix A
  !> @param [in] bandwidth_A bandwidth of the banded matrix
  !> @param [in] alpha_A banded matrix parameter \f$\alpha\f$
  !> @param [in] beta_A banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m_A banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p_A banded matrix parameter \f$\gamma_+\f$
  !> @param [in] nrow_B number of rows in the banded matrix B
  !> @param [in] ncol_B number of columns in the banded matrix B
  !> @param [in] bandwidth_B bandwidth of the banded matrix
  !> @param [in] alpha_B banded matrix parameter \f$\alpha\f$
  !> @param [in] beta_B banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m_B banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p_B banded matrix parameter \f$\gamma_+\f$
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
                                           columnwise_matrix_B,       &
                                           columnwise_matrix_C,       &
                                           nrow_A, ncol_A,            &
                                           bandwidth_A,               &
                                           alpha_A,                   &
                                           beta_A,                    &
                                           gamma_m_A,                 &
                                           gamma_p_A,                 &
                                           nrow_B, ncol_B,            &
                                           bandwidth_B,               &
                                           alpha_B,                   &
                                           beta_B,                    &
                                           gamma_m_B,                 &
                                           gamma_p_B,                 &
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

  end subroutine columnwise_op_mul_kernel_code

end module columnwise_op_mul_kernel_mod
