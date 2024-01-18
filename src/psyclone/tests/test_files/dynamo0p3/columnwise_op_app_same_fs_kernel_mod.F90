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

!> @brief Kernel which (incrementally) applies a columnwise assembled operator to a field

module columnwise_op_app_same_fs_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                    &
                                    GH_FIELD, GH_COLUMNWISE_OPERATOR,       &
                                    GH_REAL, GH_READ, GH_INC,               &
                                    ANY_SPACE_1, ANY_SPACE_2,               &
                                    CELL_COLUMN

use constants_mod,           only : r_def, r_solver, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_app_same_fs_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                               &
       arg_type(GH_FIELD,               GH_REAL, GH_INC,  ANY_SPACE_2),             &
       arg_type(GH_FIELD,               GH_REAL, GH_READ, ANY_SPACE_2),             &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ, ANY_SPACE_2, ANY_SPACE_2) &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: columnwise_op_app_same_fs_kernel_code
end type columnwise_op_app_same_fs_kernel_type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_app_same_fs_kernel_code

contains

  !> @param [in] cell Horizontal cell index
  !> @param [in] ncell_2d Number of cells in 2d grid
  !> @param [in,out] lhs Resulting field lhs += A.x
  !> @param [in] x Input field
  !> @param [in] columnwise_matrix Banded matrix to assemble into
  !> @param [in] nrow Number of rows in the banded matrix
  !> @param [in] bandwidth Bandwidth of the banded matrix
  !> @param [in] alpha Banded matrix parameter \f$\alpha\f$
  !> @param [in] beta Banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m Banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p Banded matrix parameter \f$\gamma_+\f$
  !> @param [in] ndf_aspc2 Number of degrees of freedom per cell for the
  !!                       to- and from-space
  !> @param [in] undf_aspc2 Unique number of degrees of freedom for the
  !!                        to- and from-space
  !> @param [in] map_aspc2 Dofmap for the to-and from-space
  !> @param [in] indirection_dofmap_to Indirection map for to-space
  subroutine columnwise_op_app_same_fs_kernel_code(cell, ncell_2d,        &
                                                   lhs, x,                &
                                                   columnwise_matrix,     &
                                                   nrow, bandwidth,       &
                                                   alpha, beta,           &
                                                   gamma_m, gamma_p,      &
                                                   ndf_aspc2, undf_aspc2, &
                                                   map_aspc2, indirection_dofmap_to)

    implicit none

    integer(kind=i_def), intent(in) :: cell, ncell_2d
    integer(kind=i_def), intent(in) :: nrow
    integer(kind=i_def), intent(in) :: bandwidth, alpha, beta, gamma_m, gamma_p
    integer(kind=i_def), intent(in) :: undf_aspc2
    integer(kind=i_def), intent(in) :: ndf_aspc2
    integer(kind=i_def), intent(in), dimension(ndf_aspc2) :: map_aspc2
    integer(kind=i_def), intent(in), dimension(nrow) :: indirection_dofmap_to
    real(kind=r_solver), intent(in), dimension(bandwidth,nrow,ncell_2d) :: columnwise_matrix
    real(kind=r_def), intent(inout), dimension(undf_aspc2) :: lhs
    real(kind=r_def), intent(in), dimension(undf_aspc2)    :: x

  end subroutine columnwise_op_app_same_fs_kernel_code

end module columnwise_op_app_same_fs_kernel_mod
