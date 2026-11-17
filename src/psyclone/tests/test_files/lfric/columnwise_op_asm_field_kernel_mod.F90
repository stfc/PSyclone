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

module columnwise_op_asm_field_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type, GH_FIELD,          &
                                    GH_OPERATOR, GH_COLUMNWISE_OPERATOR,    &
                                    GH_REAL, GH_READ, GH_WRITE,             &
                                    ANY_SPACE_1, ANY_SPACE_2,               &
                                    CELL_COLUMN

use constants_mod,           only : r_def, r_solver, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_asm_field_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                                 &
       arg_type(GH_FIELD,               GH_REAL, GH_READ,  ANY_SPACE_1),              &
       arg_type(GH_OPERATOR,            GH_REAL, GH_READ,  ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_WRITE, ANY_SPACE_1, ANY_SPACE_2)  &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: columnwise_op_asm_field_kernel_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_asm_field_kernel_code

contains

  subroutine columnwise_op_asm_field_kernel_code(cell, nlayers, ncell_2d,     &
                                     field1, ncell_3d, op_2, cma_op_3,        &
                                     cma_op_3_nrow,   cma_op_3_ncol,          &
                                     cma_op_3_bandwidth, cma_op_3_alpha,      &
                                     cma_op_3_beta, cma_op_3_gamma_m,         &
                                     cma_op_3_gamma_p, ndf_aspc1, undf_aspc1, &
                                     map_aspc1, cbanded_map_aspc1, ndf_aspc2, &
                                     cbanded_map_aspc2)

    implicit none

    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_2d
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: undf_aspc1
    integer(kind=i_def), intent(in) :: ndf_aspc2
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: cma_op_3_nrow, cma_op_3_ncol,       &
                                       cma_op_3_bandwidth, cma_op_3_alpha, &
                                       cma_op_3_beta, cma_op_3_gamma_m,    &
                                       cma_op_3_gamma_p
    integer(kind=i_def), intent(in), dimension(ndf_aspc1) :: map_aspc1
    integer(kind=i_def), intent(in), dimension(ndf_aspc1,nlayers) :: cbanded_map_aspc1
    integer(kind=i_def), intent(in), dimension(ndf_aspc2,nlayers) :: cbanded_map_aspc2
    real(kind=r_def), intent(in), dimension(undf_aspc1) :: field1
    real(kind=r_def), intent(in), dimension(ncell_3d,ndf_aspc1,ndf_aspc2) :: op_2
    real(kind=r_solver), intent(inout), dimension(cma_op_3_bandwidth, cma_op_3_nrow,ncell_2d) :: cma_op_3

    write (*,*) "Hello CMA World"

  end subroutine columnwise_op_asm_field_kernel_code

end module columnwise_op_asm_field_kernel_mod
