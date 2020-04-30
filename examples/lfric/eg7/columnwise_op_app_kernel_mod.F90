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
!> @brief Kernel which (incrementally) applies a columnwise assembled operator
!>        to a field
module columnwise_op_app_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                    &
                                    GH_FIELD, GH_COLUMNWISE_OPERATOR,       &
                                    GH_READ, GH_INC,                        &
                                    ANY_SPACE_1, ANY_SPACE_2,               &
                                    GH_COLUMN_INDIRECTION_DOFMAP,           &
                                    CELLS

use constants_mod,           only : r_def, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_app_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                      &
       arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1),                        &
       arg_type(GH_FIELD,    GH_READ, ANY_SPACE_2),                        &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2) &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: columnwise_op_app_kernel_code
end type columnwise_op_app_kernel_type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_app_kernel_code

contains

  !> @brief The subroutine which is called directly from the PSY layer and
  !> applies the operator as lhs += A.x
  !>
  !> @param [in] cell the horizontal cell index
  !> @param [in] ncell_2d number of cells in 2d grid
  !> @param [in,out] lhs Resulting field lhs += A.x
  !> @param [in] x input field
  !> @param [in] columnwise_matrix banded matrix to assemble into
  !> @param [in] nrow number of rows in the banded matrix
  !> @param [in] ncol number of columns in the banded matrix
  !> @param [in] bandwidth bandwidth of the banded matrix
  !> @param [in] alpha banded matrix parameter \f$\alpha\f$
  !> @param [in] beta banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p banded matrix parameter \f$\gamma_+\f$
  !> @param [in] ndf1 number of degrees of freedom per cell for the to-space
  !> @param [in] undf1 unique number of degrees of freedom  for the to-space
  !> @param [in] map1 dofmap for the to-space
  !> @param [in] indirection_dofmap_to indirection map for to-space
  !> @param [in] ndf2 number of degrees of freedom per cell for the from-space
  !> @param [in] undf2 unique number of degrees of freedom for the from-space
  !> @param [in] map2 dofmap for the from-space
  !> @param [in] indirection_dofmap_from indirection map for from-space
  subroutine columnwise_op_app_kernel_code(cell,                  &
                                           ncell_2d,              &
                                           lhs, x,                &
                                           columnwise_matrix,     &
                                           nrow,                  &
                                           ncol,                  &
                                           bandwidth,             &
                                           alpha,                 &
                                           beta,                  &
                                           gamma_m,               &
                                           gamma_p,               &
                                           ndf1, undf1, map1,     &
                                           indirection_dofmap_to, &
                                           ndf2, undf2, map2,     &
                                           indirection_dofmap_from)

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: cell,  ncell_2d
    integer(kind=i_def), intent(in) :: nrow, ncol, bandwidth
    integer(kind=i_def), intent(in) :: undf1, ndf1
    integer(kind=i_def), intent(in) :: undf2, ndf2
    real(kind=r_def), dimension(undf1), intent(inout) :: lhs
    real(kind=r_def), dimension(undf2), intent(in) :: x
    real(kind=r_def), dimension(bandwidth,nrow,ncell_2d), intent(in) :: columnwise_matrix
    integer(kind=i_def), dimension(ndf1), intent(in) :: map1
    integer(kind=i_def), dimension(ndf2), intent(in) :: map2

    integer(kind=i_def), intent(in) :: alpha, beta, gamma_m, gamma_p
    integer(kind=i_def), dimension(nrow), intent(in) :: indirection_dofmap_to
    integer(kind=i_def), dimension(ncol), intent(in) :: indirection_dofmap_from

    ! Internal parameters
    ! Row and column index index
    integer(kind=i_def) :: i,j, mu_i,mu_j
    ! Smallest/largest index in a particular row
    integer(kind=i_def) :: j_minus, j_plus

    do i=1, nrow
       ! Assumes that the first entry in the dofmaps is the smallest
       mu_i = map1(1) + indirection_dofmap_to(i) - 1
       j_minus = ceiling((alpha*i-gamma_p)/(1.0_r_def*beta),i_def)
       j_plus = floor((alpha*i+gamma_m)/(1.0_r_def*beta),i_def)
       do j=MAX(1,j_minus), MIN(ncol,j_plus)
          mu_j = map2(1) + indirection_dofmap_from(j) - 1
          lhs(mu_i) = lhs(mu_i) &
                    + columnwise_matrix(j-j_minus+1,i,cell) * x(mu_j)
       end do
    end do

  end subroutine columnwise_op_app_kernel_code

end module columnwise_op_app_kernel_mod
