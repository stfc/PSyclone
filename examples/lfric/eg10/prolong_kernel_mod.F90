!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Perform the prolongation operation from a coarse grid field to a fine
!!        grid field
!> @details Prolong the coarse grid correction into all cells on the fine grid
!!          that are contained in that coarse grid cell. Values are assumed to
!!          be pointwise scalar values so no area weighting is used

module prolong_kernel_mod

use constants_mod,           only: i_def, r_def
use kernel_mod,              only: kernel_type
use argument_mod,            only: arg_type,                  &
                                   GH_FIELD, CELLS,           &
                                   GH_READ, GH_READWRITE,     &
                                   ANY_DISCONTINUOUS_SPACE_1, &
                                   ANY_DISCONTINUOUS_SPACE_2, &
                                   GH_COARSE, GH_FINE

implicit none

private

type, public, extends(kernel_type) :: prolong_kernel_type
   private
   type(arg_type) :: meta_args(2) = (/                                                  &
       arg_type(GH_FIELD, GH_READWRITE, ANY_DISCONTINUOUS_SPACE_1, mesh_arg=GH_FINE),   &
       arg_type(GH_FIELD, GH_READ,      ANY_DISCONTINUOUS_SPACE_2, mesh_arg=GH_COARSE ) &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: prolong_kernel_code
end type prolong_kernel_type

public :: prolong_kernel_code

contains

  !> @brief Subroutine to perform the prolongation operation
  !> @param[in] nlayers Number of layers in a model column
  !> @param[in] cell_map Map of which fine grid cells lie in the coarse grid
  !!                     cell
  !> @param[in] ncell_f_per_c_x Number of fine cells per coarse cell in x-direction
  !> @param[in] ncell_f_per_c_y Number of fine cells per coarse cell in y-direction
  !> @param[in] ncell_f Number of cells in the fine grid
  !> @param[in,out] fine Fine grid field to update
  !> @param[in] coarse Coarse grid field to prolong
  !> @param[in] ndf Number of degrees of freedom per cell on both the coarse and
  !!                fine grid
  !> @param[in] undf_f Total number of degrees of freedom on the fine grid
  !> @param[in] dofmap_f Cell dofmap on the fine grid
  !> @param[in] undf_c Total number of degrees of freedom on the coarse grid
  !> @param[in] dofmap_c Cell dofmap on the coarse grid
  subroutine prolong_kernel_code(nlayers,         &
                                 cell_map,        &
                                 ncell_f_per_c_x, &
                                 ncell_f_per_c_y, &
                                 ncell_f,         &
                                 fine,            &
                                 coarse,          &
                                 ndf,             &
                                 undf_f,          &
                                 dofmap_f,        &
                                 undf_c,          &
                                 dofmap_c)

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
    real(kind=r_def), dimension(undf_c), intent(in)    :: coarse
    real(kind=r_def), dimension(undf_f), intent(inout) :: fine

    integer(kind=i_def) :: df, k, lp_x, lp_y

    do k = 0, nlayers-1
      do df = 1, ndf
        do lp_y = 1, ncell_f_per_c_y
          do lp_x = 1, ncell_f_per_c_x
            fine(dofmap_f(df,cell_map(lp_x,lp_y))+k) &
                  = fine(dofmap_f(df,cell_map(lp_x,lp_y))+k) + coarse(dofmap_c(df)+k)
          end do
        end do
      end do
    end do

  end subroutine prolong_kernel_code

end module prolong_kernel_mod
