!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @details Kernel to compute the coordinates fields at nodal points of another
!> function space. In general this will give a polynomial approximation to the
!> mesh, i.e. if the mesh is spherical then the nodal coordinates will not lie on
!> spherical shells but will instead represent a polynomial approximation to the
!> spherical shell.

module nodal_coordinates_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_READ, GH_INC,               &
                                    ANY_SPACE_9, ANY_SPACE_1,                &
                                    GH_BASIS,                                &
                                    CELLS,                                   &
                                    GH_EVALUATOR
use constants_mod,           only : r_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
! The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: nodal_coordinates_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/                                  &
       arg_type(GH_FIELD*3,   GH_INC,   ANY_SPACE_1),                  &
       arg_type(GH_FIELD*3,   GH_READ,  ANY_SPACE_9)                   &
       /)
  type(func_type) :: meta_funcs(1) = (/                                &
       func_type(ANY_SPACE_9, GH_BASIS)                                &
       /)
  integer :: iterates_over = CELLS
  integer :: gh_shape = GH_EVALUATOR
contains
  procedure, nopass ::nodal_coordinates_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public nodal_coordinates_code
contains

!> @brief   Compute the coordinates fields at nodal points of another
!>          function space
!>@param[in] nlayers Number of layers
!>@param[in] ndf_x Number of degrees of freedom per cell for the output field
!>@param[in] undf_x Number of unique degrees of freedom for the output field
!>@param[in] map_x Dofmap for the cell at the base of the column for the output field
!>@param[in] ndf_chi Number of degrees of freedom per cell for the input field
!>@param[in] undf_chi Number of unique degrees of freedom for the input field
!>@param[in] map_chi Dofmap for the cell at the base of the column for the input field
!>@param[out] nodal_x Nodal coordinates in the first direction
!>@param[out] nodal_y Nodal coordinates in the second direction
!>@param[out] nodal_z Nodal coordinates in the third direction
!>@param[in] chi1 Coordinates in the first direction
!>@param[in] chi2 Coordinates in the second direction
!>@param[in] chi3 Coordinates in the third direction
!>@param[in] basis_chi Basis functions of the chi function space evaluated at the nodal points of the x function space
subroutine nodal_coordinates_code(nlayers,                                    &
                                  nodal_x, nodal_y, nodal_z,                  &
                                  chi1, chi2, chi3,                           &
                                  ndf_x, undf_x, map_x,                       &
                                  ndf_chi, undf_chi, map_chi,                 &
                                  basis_chi                                   &
                                  )

  implicit none

  ! Arguments
  integer, intent(in) :: nlayers
  integer, intent(in) :: ndf_x, ndf_chi, undf_x, undf_chi
  integer, dimension(ndf_x),   intent(in) :: map_x
  integer, dimension(ndf_chi), intent(in) :: map_chi
  real(kind=r_def), dimension(undf_x),        intent(out) :: nodal_x, nodal_y, nodal_z
  real(kind=r_def), dimension(undf_chi),      intent(in)  :: chi1, chi2, chi3
  real(kind=r_def), dimension(1,ndf_chi,ndf_x), intent(in)  :: basis_chi

  ! Internal variables
  integer          :: df_x, df_chi, k
  real(kind=r_def) :: xyz(3)

  do k = 0, nlayers-1
    do df_x = 1,ndf_x
      xyz(:) = 0.0_r_def
      do df_chi = 1, ndf_chi
        xyz(1) = xyz(1) + chi1(map_chi(df_chi)+k)*basis_chi(1,df_chi,df_x)
        xyz(2) = xyz(2) + chi2(map_chi(df_chi)+k)*basis_chi(1,df_chi,df_x)
        xyz(3) = xyz(3) + chi3(map_chi(df_chi)+k)*basis_chi(1,df_chi,df_x)
      end do
      nodal_x(map_x(df_x)+k) = xyz(1)
      nodal_y(map_x(df_x)+k) = xyz(2)
      nodal_z(map_x(df_x)+k) = xyz(3)
    end do
  end do

end subroutine nodal_coordinates_code

end module nodal_coordinates_kernel_mod
