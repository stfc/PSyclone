!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
! -----------------------------------------------------------------------------
!
! BSD 3-Clause License
!
! Modifications copyright (c) 2019-2021, Science and Technology Facilities Council
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
! Modified A. R. Porter, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

!> @brief Perform the restriction operation from a fine grid field to a coarse
!!        grid field
!> @details Restrict the fine grid field over a number of cells into a single
!!          coarse grid field. The fine grid cells are considered to be
!!          contained in a coarse grid cell. Values are assumed to
!!          be pointwise scalar values so no area weighting is used
module restrict_kernel_mod

use constants_mod,           only: i_def, r_def
use kernel_mod,              only: kernel_type
use argument_mod,            only: arg_type,                  &
                                   GH_FIELD, GH_REAL,         &
                                   CELL_COLUMN,               &
                                   GH_READ, GH_WRITE,         &
                                   ANY_DISCONTINUOUS_SPACE_1, &
                                   ANY_DISCONTINUOUS_SPACE_2, &
                                   GH_COARSE, GH_FINE

implicit none

private

type, public, extends(kernel_type) :: restrict_kernel_type
   private
   type(arg_type) :: meta_args(2) = (/                                   &
        arg_type(GH_FIELD, GH_REAL, GH_WRITE, ANY_DISCONTINUOUS_SPACE_1, &
                                              mesh_arg=GH_COARSE),       &
        arg_type(GH_FIELD, GH_REAL, GH_READ,  ANY_DISCONTINUOUS_SPACE_2, &
                                              mesh_arg=GH_FINE)          &
        /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: restrict_kernel_code
end type restrict_kernel_type

public :: restrict_kernel_code

contains

  !> @brief Subroutine to perform the restriction operation
  !> @param[in] nlayers Number of layers in a model column
  !> @param[in] cell_map Map of which fine grid cells lie in the coarse grid
  !!                     cell
  !> @param[in] ncell_f_per_c_x Number of fine cells per coarse cell in x-direction
  !> @param[in] ncell_f_per_c_y Number of fine cells per coarse cell in y-direction
  !> @param[in] ncell_f  Number of cells in the fine grid
  !> @param[in,out] coarse Coarse grid field to compute
  !> @param[in] fine Fine grid field to restrict
  !> @param[in] undf_c Total number of degrees of freedom on the coarse grid
  !> @param[in] dofmap_c Cell dofmap on the coarse grid
  !> @param[in] ndf Number of degrees of freedom per cell on both the coarse and
  !!                fine grid
  !> @param[in] undf_f Total number of degrees of freedom on the fine grid
  !> @param[in] dofmap_f Cell dofmap on the fine grid
  subroutine restrict_kernel_code(nlayers,         &
                                  cell_map,        &
                                  ncell_f_per_c_x, &
                                  ncell_f_per_c_y, &
                                  ncell_f,         &
                                  coarse,          &
                                  fine,            &
                                  undf_c,          &
                                  dofmap_c,        &
                                  ndf,             &
                                  undf_f,          &
                                  dofmap_f)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_f_per_c_x
    integer(kind=i_def), intent(in) :: ncell_f_per_c_y
    integer(kind=i_def), dimension(ncell_f_per_c_x, ncell_f_per_c_y), intent(in) :: cell_map
    integer(kind=i_def), intent(in) :: ncell_f
    integer(kind=i_def), intent(in) :: ndf
    integer(kind=i_def), dimension(ndf, ncell_f), intent(in) :: dofmap_f
    integer(kind=i_def), dimension(ndf), intent(in) :: dofmap_c
    integer(kind=i_def), intent(in) :: undf_f, undf_c
    real(kind=r_def), dimension(undf_c), intent(inout) :: coarse
    real(kind=r_def), dimension(undf_f), intent(in) :: fine

    integer(kind=i_def) :: df, k, lp_x, lp_y
    real(kind=r_def) :: denom

    denom = 1.0_r_def/real(ncell_f_per_c_x*ncell_f_per_c_y, kind=r_def)

    do k = 0, nlayers-1
      do df = 1, ndf
        coarse(dofmap_c(df) + k ) = 0.0_r_def
        do lp_y = 1, ncell_f_per_c_y
          do lp_x = 1, ncell_f_per_c_x
            coarse(dofmap_c(df) + k ) = coarse(dofmap_c(df) + k ) + &
                                        fine(dofmap_f(df,cell_map(lp_x,lp_y))+k)*denom
          end do
        end do
      end do
    end do

  end subroutine restrict_kernel_code

end module restrict_kernel_mod
