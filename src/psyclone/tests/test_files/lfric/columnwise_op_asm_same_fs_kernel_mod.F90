! -----------------------------------------------------------------------------
! Original under:
! Copyright (c) 2017-2026, Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! Modifications under:
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> @brief Kernel which assembles a locally assembled matrix (LMA) into a
!! columnwise assembled matrix (CMA). Takes a read-only field as argument too.

module columnwise_op_asm_same_fs_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                  &
                                    GH_OPERATOR, GH_COLUMNWISE_OPERATOR,  &
                                    GH_FIELD, GH_REAL, GH_READ, GH_WRITE, &
                                    ANY_SPACE_1, ANY_SPACE_2, CELL_COLUMN

use constants_mod,           only : r_def, r_solver, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_asm_same_fs_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                                 &
       arg_type(GH_OPERATOR,            GH_REAL, GH_READ,  ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_FIELD,               GH_REAL, GH_READ,  ANY_SPACE_1),              &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_WRITE, ANY_SPACE_2, ANY_SPACE_2)  &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: columnwise_op_asm_same_fs_kernel_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_asm_same_fs_kernel_code

contains

  !> @brief Assembles the LMA into a CMA
  !> @details Given an LMA representation of the operator mapping between two
  !!          horizontally continuous spaces, assemble the columnwise matrix
  !!          representation of the operator.
  !>
  !> @param [in] cell Horizontal cell index
  !> @param [in] nlayers Number of vertical layers
  !> @param [in] ncell_2d Number of cells in 2d grid
  !> @param [in] ncell_3d Total number of cells
  !> @param [in] local_stencil Locally assembled matrix
  !> @param [in] field Field argument of locally assembled matrix
  !> @param [in,out] columnwise_matrix Banded matrix to assemble into
  !> @param [in] nrow Number of rows in the banded matrix
  !> @param [in] bandwidth Bandwidth of the banded matrix
  !> @param [in] alpha Banded matrix parameter \f$\alpha\f$
  !> @param [in] beta Banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m Banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p Banded matrix parameter \f$\gamma_+\f$
  !> @param [in] ndf_lma_to Number of dofs per cell for the LMA to-space
  !> @param [in] undf_lma_to Number of unique dofs for the F-S that the field is on
  !> @param [in] map_lma_to Dofmap for the F-S that the field is on
  !> @param [in] ndf_lma_from Number of dofs per cell for the LMA from-space
  !> @param [in] column_banded_dofmap_to List of offsets for to/from-space
  subroutine columnwise_op_asm_same_fs_kernel_code(cell,              &
                                                   nlayers,           &
                                                   ncell_2d,          &
                                                   ncell_3d,          &
                                                   local_stencil,     &
                                                   field,             &
                                                   columnwise_matrix, &
                                                   nrow,              &
                                                   bandwidth,         &
                                                   alpha,             &
                                                   beta,              &
                                                   gamma_m,           &
                                                   gamma_p,           &
                                                   ndf_lma_to,        & ! any_space_1
                                                   undf_lma_to,       &
                                                   map_lma_to,        &
                                                   ndf_lma_from,      & ! any_space_2
                                                   column_banded_dofmap_to)

    implicit none
    
    ! Arguments
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_lma_to
    integer(kind=i_def), intent(in) :: ndf_lma_from
    integer(kind=i_def), intent(in) :: undf_lma_to
    integer(kind=i_def), intent(in) :: cell, ncell_2d
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: nrow, bandwidth, alpha, beta, gamma_m, gamma_p
    integer(kind=i_def), intent(in), dimension(ndf_lma_to) :: map_lma_to
    integer(kind=i_def), intent(in), dimension(ndf_lma_from,nlayers) :: column_banded_dofmap_to
    real(kind=r_solver), intent(inout), dimension(bandwidth,nrow,ncell_2d) :: columnwise_matrix
    real(kind=r_def), intent(in), dimension(undf_lma_to) :: field
    real(kind=r_def), intent(in), dimension(ncell_3d,ndf_lma_to,ndf_lma_from) :: local_stencil

    write (*,*) "Hello CMA World"

  end subroutine columnwise_op_asm_same_fs_kernel_code

end module columnwise_op_asm_same_fs_kernel_mod
