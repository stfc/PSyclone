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


!> @brief Kernel which assembles a locally assembled matrix (LMA) into a
!!        columnwise assembled matrix (CMA)

module columnwise_op_asm_kernel_scalar_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                   &
                                    GH_OPERATOR, GH_COLUMNWISE_OPERATOR,   &
                                    GH_SCALAR, GH_REAL, GH_READ, GH_WRITE, &
                                    ANY_SPACE_1, ANY_SPACE_2, CELL_COLUMN

use constants_mod,           only : r_def, r_solver, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_asm_kernel_scalar_type
  private
  type(arg_type) :: meta_args(3) = (/                                                 &
       arg_type(GH_OPERATOR,            GH_REAL, GH_READ,  ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_WRITE, ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_SCALAR,              GH_REAL, GH_READ) &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: columnwise_op_asm_kernel_scalar_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_asm_kernel_scalar_code

contains

  !> @brief The subroutine which is called directly from the PSY layer and
  !!        assembles the LMA into a CMA
  !> @details Given an LMA representation of the operator mapping between two
  !!          horizontally discontinuous spaces, assemble the columnwise matrix
  !!          representation of the operator.
  !>
  !> @param [in] cell Horizontal cell index
  !> @param [in] nlayers Number of vertical layers
  !> @param [in] ncell_2d Number of cells in 2d grid
  !> @param [in] ncell_3d Total number of cells
  !> @param [in] local_stencil Locally assembled matrix
  !> @param [in,out] columnwise_matrix Banded matrix to assemble into
  !> @param [in] nrow Number of rows in the banded matrix
  !> @param [in] ncol Number of columns in the banded matrix
  !> @param [in] bandwidth Bandwidth of the banded matrix
  !> @param [in] alpha Banded matrix parameter \f$\alpha\f$
  !> @param [in] beta Banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m Banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p Banded matrix parameter \f$\gamma_+\f$
  !> @param [in] a_scalar Example scalar argument
  !> @param [in] ndf_to Number of degrees of freedom per cell for the to-space
  !> @param [in] column_banded_dofmap_to List of offsets for to-space
  !> @param [in] ndf_from Number of degrees of freedom per cell for the from-space
  !> @param [in] column_banded_dofmap_from List of offsets for from-space
  subroutine columnwise_op_asm_kernel_scalar_code(cell,                    &
                                                  nlayers,                 &
                                                  ncell_2d,                &
                                                  ncell_3d,                &
                                                  local_stencil,           &
                                                  columnwise_matrix,       &
                                                  nrow,                    &
                                                  ncol,                    &
                                                  bandwidth,               &
                                                  alpha,                   &
                                                  beta,                    &
                                                  gamma_m,                 &
                                                  gamma_p,                 &
                                                  a_scalar,                &
                                                  ndf_to,                  &
                                                  column_banded_dofmap_to, &
                                                  ndf_from,                &
                                                  column_banded_dofmap_from)

    implicit none
    
    ! Arguments
    integer(kind=i_def), intent(in) :: cell,  nlayers, ncell_3d, ncell_2d
    integer(kind=i_def), intent(in) :: nrow, ncol, bandwidth
    integer(kind=i_def), intent(in) :: ndf_to, ndf_from
    integer(kind=i_def), intent(in) :: alpha, beta, gamma_m, gamma_p
    integer(kind=i_def), dimension(ndf_to,nlayers), intent(in)   :: column_banded_dofmap_to
    integer(kind=i_def), dimension(ndf_from,nlayers), intent(in) :: column_banded_dofmap_from
    real(kind=r_def), intent(in) :: a_scalar
    real(kind=r_def), dimension(ndf_to,ndf_from,ncell_3d), intent(in) :: local_stencil
    real(kind=r_solver), dimension(bandwidth,nrow,ncell_2d), intent(inout) :: columnwise_matrix
    
    write (*,*) "Hello CMA World"

  end subroutine columnwise_op_asm_kernel_scalar_code

end module columnwise_op_asm_kernel_scalar_mod
