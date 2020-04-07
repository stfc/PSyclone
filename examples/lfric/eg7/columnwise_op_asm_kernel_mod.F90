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
!> @brief Kernel which assembles a locally assembled matrix (LMA) into a
!>        columnwise assembled matrix (CMA)
module columnwise_op_asm_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                    &
                                    GH_OPERATOR, GH_COLUMNWISE_OPERATOR,    &
                                    GH_READ, GH_WRITE,                      &
                                    ANY_SPACE_1, ANY_SPACE_2,               &
                                    GH_COLUMN_BANDED_DOFMAP,                &
                                    CELLS

use constants_mod,           only : r_def, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_asm_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/                                       &
       arg_type(GH_OPERATOR,            GH_READ,  ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_WRITE, ANY_SPACE_1, ANY_SPACE_2) &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: columnwise_op_asm_kernel_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_asm_kernel_code

contains

  !> @brief The subroutine which is called directly from the PSY layer and
  !> assembles the LMA into a CMA
  !> @details Given an LMA representation of the operator mapping between two
  !> horizontally discontinuous spaces, assemble the columnwise matrix
  !> representation of the operator.
  !>
  !> @param [in] cell the horizontal cell index
  !> @param [in] nlayers number of vertical layers
  !> @param [in] ncell_2d number of cells in 2d grid
  !> @param [in] ncell_3d total number of cells
  !> @param [in] local_stencil locally assembled matrix
  !> @param [out] columnwise_matrix banded matrix to assemble into
  !> @param [in] nrow number of rows in the banded matrix
  !> @param [in] ncol number of columns in the banded matrix
  !> @param [in] bandwidth bandwidth of the banded matrix
  !> @param [in] alpha banded matrix parameter \f$\alpha\f$
  !> @param [in] beta banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p banded matrix parameter \f$\gamma_+\f$
  !> @param [in] ndf_to number of degrees of freedom per cell for the to-space
  !> @param [in] column_banded_dofmap_to list of offsets for to-space
  !> @param [in] ndf_from number of degrees of freedom per cell for the from-sp
  !> @param [in] column_banded_dofmap_from list of offsets for from-space
  subroutine columnwise_op_asm_kernel_code(cell,                     &
                                           nlayers,                  &
                                           ncell_2d,                 &
                                           ncell_3d,                 &
                                           local_stencil,            &
                                           columnwise_matrix,        &
                                           nrow,                     &
                                           ncol,                     &
                                           bandwidth,                &
                                           alpha,                    &
                                           beta,                     &
                                           gamma_m,                  &
                                           gamma_p,                  &
                                           ndf_to,                   &
                                           column_banded_dofmap_to,  &
                                           ndf_from,                 &
                                           column_banded_dofmap_from)

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: cell,  nlayers, ncell_3d, ncell_2d
    real(kind=r_def), dimension(ndf_to,ndf_from,ncell_3d), intent(in) :: local_stencil
    integer(kind=i_def), intent(in) :: nrow, ncol, bandwidth
    real(kind=r_def), dimension(bandwidth,nrow,ncell_2d), intent(out) :: columnwise_matrix
    integer(kind=i_def), intent(in) :: ndf_to, ndf_from
    integer(kind=i_def), intent(in) :: alpha, beta, gamma_m, gamma_p
    integer(kind=i_def), dimension(ndf_to,nlayers), intent(in) :: column_banded_dofmap_to
    integer(kind=i_def), dimension(ndf_from,nlayers), intent(in) :: column_banded_dofmap_from

    ! Internal parameters
    integer(kind=i_def) :: df1, df2 ! loop indices for dofs
    integer(kind=i_def) :: i,j ! Row and column index index
    integer(kind=i_def) :: j_minus ! First column in a row
    integer(kind=i_def) :: ik !ncell3d counter
    integer(kind=i_def) :: k !nlayers  counter

    k = gamma_m+ ncol

    ! Initialise matrix to zero
    columnwise_matrix( :, :, cell ) = 0.0_r_def
    ! Loop over all vertical layers
    do k=1, nlayers
       ik = (cell-1)*nlayers + k ! cell index in 3d
       do df1=1, ndf_to
          i = column_banded_dofmap_to( df1, k )
          j_minus = ceiling((alpha*i-gamma_p)/(1.0_8*beta),i_def)
          do df2=1, ndf_from
             j = column_banded_dofmap_from( df2, k )
             columnwise_matrix( j-j_minus+1, i, cell )      &
                  = columnwise_matrix( j-j_minus+1, i, cell ) &
                  + local_stencil( df1 ,df2, ik )
         end do
       end do
    end do

  end subroutine columnwise_op_asm_kernel_code

end module columnwise_op_asm_kernel_mod
