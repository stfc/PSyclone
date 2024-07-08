!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel to compute the apply the div conforming Piola transform to a
!! computational vector field and return the 3 components of the physical field as
!! separate fields in the target space

! This tweaked version of the the code 1) replaces ANY_SPACE_9 with
! WCHI and 2) splits matmul onto a separate line.

module convert_hdiv_field_kernel_mod

use kernel_mod,              only : kernel_type
use fs_continuity_mod,       only : w3, wchi
use argument_mod,            only : arg_type, func_type,       &
                                    GH_FIELD, GH_REAL, GH_INC, &
                                    GH_READ,                   &
                                    ANY_SPACE_2, ANY_SPACE_1,  &
                                    ANY_DISCONTINUOUS_SPACE_3, &
                                    GH_DIFF_BASIS, GH_BASIS,   &
                                    CELL_COLUMN, GH_EVALUATOR
use constants_mod,           only : r_def, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: convert_hdiv_field_kernel_type
  private
  type(arg_type) :: meta_args(4) = (/                                    &
       arg_type(GH_FIELD*3, GH_REAL, GH_INC,  ANY_SPACE_1),              &
       arg_type(GH_FIELD,   GH_REAL, GH_READ, ANY_SPACE_2),              &
       arg_type(GH_FIELD*3, GH_REAL, GH_READ, WCHI),                     &
       arg_type(GH_FIELD,   GH_REAL, GH_READ, ANY_DISCONTINUOUS_SPACE_3) &
       /)
  type(func_type) :: meta_funcs(2) = (/                                  &
       func_type(ANY_SPACE_2, GH_BASIS),                                 &
       func_type(WCHI, GH_BASIS, GH_DIFF_BASIS)                          &
       /)
  integer :: operates_on = CELL_COLUMN
  integer :: gh_shape = GH_EVALUATOR
contains
  procedure, nopass :: convert_hdiv_field_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: convert_hdiv_field_code
contains

!> @param[in] nlayers Number of layers
!> @param[in] ndf Number of degrees of freedom per cell for the output field
!> @param[in] undf Number of unique degrees of freedom for the output field
!> @param[in] map Dofmap for the cell at the base of the column for the output field
!> @param[in,out] physical_field1 First component of the output field in physical units
!> @param[in,out] physical_field2 Second component of the  output field in physical units
!> @param[in,out] physical_field3 Third component of the  output field in physical units
!> @param[in] computational_field Output field in computational units
!> @param[in] chi1 Coordinates in the first direction
!> @param[in] chi2 Coordinates in the second direction
!> @param[in] chi3 Coordinates in the third direction
!> @param[in] panel_id A field giving the ID for mesh panels
!> @param[in] ndf1 Number of degrees of freedom per cell for the physical field
!> @param[in] undf1 Number of unique degrees of freedom for the physical field
!> @param[in] map1 Dofmap for the cell at the base of the column for the physical field
!> @param[in] ndf2 Number of degrees of freedom per cell for the computational field
!> @param[in] undf2 Number of unique degrees of freedom for the computational field
!> @param[in] map2 Dofmap for the cell at the base of the column for the computational field
!> @param[in] basis2 Basis functions for the computational field evaluated at
!>                   the physical field nodal points
!> @param[in] ndf_chi Number of degrees of freedom per cell for the coordinate field
!> @param[in] undf_chi Number of unique degrees of freedom for the coordinate field
!> @param[in] map_chi Dofmap for the cell at the base of the column for the coordinate field
!> @param[in] basis_chi Basis functions of the coordinate field evaluated at
!>                      the physical field nodal points
!> @param[in] diff_basis_chi Differential basis functions of the coordinate
!>                           space evaluated at the physical field nodal points
!> @param[in] ndf_pid  Number of degrees of freedom per cell for panel_id
!> @param[in] undf_pid Number of unique degrees of freedom for panel_id
!> @param[in] map_pid  Dofmap for the cell at the base of the column for panel_id
subroutine convert_hdiv_field_code(nlayers,                                  &
                                   physical_field1,                          &
                                   physical_field2,                          &
                                   physical_field3,                          &
                                   computational_field,                      &
                                   chi1, chi2, chi3,                         &
                                   panel_id,                                 &
                                   ndf1, undf1, map1,                        &
                                   ndf2, undf2, map2,                        &
                                   basis2,                                   &
                                   ndf_chi, undf_chi, map_chi,               &
                                   basis_chi, diff_basis_chi,                &
                                   ndf_pid, undf_pid, map_pid                &
                                   )
  use coordinate_jacobian_mod, only: coordinate_jacobian
  implicit none

  ! Arguments
  integer(kind=i_def),                        intent(in)    :: nlayers
  integer(kind=i_def),                        intent(in)    :: ndf1, undf1
  integer(kind=i_def),                        intent(in)    :: ndf2, undf2
  integer(kind=i_def),                        intent(in)    :: ndf_chi
  integer(kind=i_def),                        intent(in)    :: undf_chi
  integer(kind=i_def),                        intent(in)    :: ndf_pid, undf_pid

  integer(kind=i_def), dimension(ndf1),       intent(in)    :: map1
  integer(kind=i_def), dimension(ndf2),       intent(in)    :: map2
  integer(kind=i_def), dimension(ndf_chi),    intent(in)    :: map_chi
  integer(kind=i_def), dimension(ndf_pid),    intent(in)    :: map_pid

  real(kind=r_def), dimension(undf2),         intent(in)    :: computational_field
  real(kind=r_def), dimension(undf_chi),      intent(in)    :: chi1, chi2, chi3
  real(kind=r_def), dimension(undf_pid),      intent(in)    :: panel_id
  real(kind=r_def), dimension(undf1),         intent(inout) :: physical_field1
  real(kind=r_def), dimension(undf1),         intent(inout) :: physical_field2
  real(kind=r_def), dimension(undf1),         intent(inout) :: physical_field3

  real(kind=r_def), dimension(1,ndf_chi,ndf1), intent(in)   :: basis_chi
  real(kind=r_def), dimension(3,ndf_chi,ndf1), intent(in)   :: diff_basis_chi
  real(kind=r_def), dimension(3,ndf2,ndf1),    intent(in)   :: basis2

  ! Internal variables
  integer(kind=i_def) :: df, df2, k
  real(kind=r_def) :: jacobian(3,3,ndf1), dj(ndf1)
  real(kind=r_def) :: vector_in(3), vector_out(3)
  real(kind=r_def), dimension(ndf_chi) :: chi1_e, chi2_e, chi3_e

  integer(kind=i_def) :: ipanel

  ipanel = int(panel_id(map_pid(1)), i_def)

  do k = 0, nlayers-1
    do df = 1,ndf_chi
      chi1_e(df) = chi1(map_chi(df) + k)
      chi2_e(df) = chi2(map_chi(df) + k)
      chi3_e(df) = chi3(map_chi(df) + k)
    end do
    call coordinate_jacobian(ndf_chi, ndf1, chi1_e, chi2_e, chi3_e,    &
                             ipanel, basis_chi, diff_basis_chi, jacobian, dj)

    do df = 1,ndf1
      vector_in(:) = 0.0_r_def
      do df2 = 1,ndf2
        vector_in(:) = vector_in(:) + computational_field(map2(df2)+k)*basis2(:,df2,df)
      end do
      vector_out(:) = matmul(jacobian(:,:,df),vector_in)
      vector_out(:) = vector_out(:)/dj(df)
      physical_field1(map1(df)+k) = physical_field1(map1(df)+k) + vector_out(1)
      physical_field2(map1(df)+k) = physical_field2(map1(df)+k) + vector_out(2)
      physical_field3(map1(df)+k) = physical_field3(map1(df)+k) + vector_out(3)
    end do
  end do

end subroutine convert_hdiv_field_code

end module convert_hdiv_field_kernel_mod
