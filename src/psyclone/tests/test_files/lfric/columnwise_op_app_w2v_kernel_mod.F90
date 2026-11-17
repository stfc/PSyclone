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

! Kernel which applies a columnwise assembled operator to a field on W2V (discontinuous)
module columnwise_op_app_w2v_kernel_mod

use kernel_mod,              only : kernel_type
use fs_continuity_mod,       only : W2V
use argument_mod,            only : arg_type, func_type,              &
                                    GH_FIELD, GH_COLUMNWISE_OPERATOR, &
                                    GH_REAL, GH_READ, GH_WRITE,       &
                                    ANY_SPACE_2, CELL_COLUMN
use constants_mod,           only : r_def, r_solver, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_app_w2v_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                        &
       arg_type(GH_FIELD,               GH_REAL, GH_WRITE, W2V),             &
       arg_type(GH_FIELD,               GH_REAL, GH_READ,  ANY_SPACE_2),     &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ,  W2V, ANY_SPACE_2) &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: columnwise_op_app_w2v_kernel_code
end type columnwise_op_app_w2v_kernel_type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_app_w2v_kernel_code

contains

  subroutine columnwise_op_app_w2v_kernel_code(cell,                    &
                                               ncell_2d,                &
                                               field1,                  &
                                               field2,                  &
                                               cma_op,                  &
                                               cma_op_nrow,             &
                                               cma_op_ncol,             &
                                               cma_op_bandwidth,        &
                                               cma_op_alpha,            &
                                               cma_op_beta,             &
                                               cma_op_gamma_m,          &
                                               cma_op_gamma_p,          &
                                               ndf_w2v,                 &
                                               undf_w2v,                &
                                               map_w2v,                 &
                                               cma_indirection_map_w2v, &
                                               ndf_aspc2,               &
                                               undf_aspc2,              &
                                               map_aspc2,               &
                                               cma_indirection_map_aspc2)

    implicit none

    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_2d
    integer(kind=i_def), intent(in) :: ndf_w2v
    integer(kind=i_def), intent(in) :: ndf_aspc2
    integer(kind=i_def), intent(in) :: undf_w2v
    integer(kind=i_def), intent(in) :: undf_aspc2
    integer(kind=i_def), intent(in) :: cma_op_nrow
    integer(kind=i_def), intent(in) :: cma_op_ncol
    integer(kind=i_def), intent(in) :: cma_op_bandwidth
    integer(kind=i_def), intent(in) :: cma_op_alpha, cma_op_beta
    integer(kind=i_def), intent(in) :: cma_op_gamma_m, cma_op_gamma_p
    integer(kind=i_def), intent(in), dimension(ndf_w2v)   :: map_w2v
    integer(kind=i_def), intent(in), dimension(ndf_aspc2) :: map_aspc2
    integer(kind=i_def), intent(in), dimension(cma_op_nrow) :: cma_indirection_map_w2v
    integer(kind=i_def), intent(in), dimension(cma_op_ncol) :: cma_indirection_map_aspc2
    real(kind=r_def), intent(inout), dimension(undf_w2v) :: field1
    real(kind=r_def), intent(in), dimension(undf_aspc2)  :: field2
    real(kind=r_solver), intent(in), dimension(cma_op_bandwidth,cma_op_nrow,ncell_2d) :: cma_op

    write(*,*) "A kernel that applies CMA operator to a field on discontinuous space W2V"

  end subroutine columnwise_op_app_w2v_kernel_code

end module columnwise_op_app_w2v_kernel_mod
