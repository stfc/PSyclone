!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel to compute the apply the curl conforming Piola transform to a
!! computational vector field and return the 3 components of the physical field as
!! separate fields

module convert_hcurl_field_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_READ, GH_INC,               &
                                    ANY_SPACE_9, ANY_SPACE_1,                         &
                                    GH_DIFF_BASIS, GH_BASIS,                 &
                                    CELLS, EVALUATOR_XYZ
use constants_mod,           only : r_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: convert_hcurl_field_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                  &
       arg_type(GH_FIELD*3,  GH_INC,  ANY_SPACE_1),                    &
       arg_type(GH_FIELD,    GH_READ, ANY_SPACE_1),                    &
       arg_type(GH_FIELD*3,  GH_READ, ANY_SPACE_9)                              &
       /)
  type(func_type) :: meta_funcs(2) = (/                                &
       func_type(ANY_SPACE_1, GH_BASIS),                               &
       func_type(ANY_SPACE_9, GH_DIFF_BASIS)                                    &
       /)
  integer :: iterates_over = CELLS
  integer :: evaluator_shape = EVALUATOR_XYZ
contains
  procedure, nopass ::convert_hcurl_field_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface convert_hcurl_field_kernel_type
   module procedure convert_hcurl_field_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public convert_hcurl_field_code
contains

type(convert_hcurl_field_kernel_type) function convert_hcurl_field_kernel_constructor() result(self)
  return
end function convert_hcurl_field_kernel_constructor

!> @param[in] nlayers Number of layers
!> @param[in] ndf Number of degrees of freedom per cell for the output field
!> @param[in] undf Number of unique degrees of freedom for the output field
!> @param[in] map Dofmap for the cell at the base of the column for the output field
!> @param[inout] physical_field1 First component of the output field in physical units
!> @param[inout] physical_field2 Second component of the  output field in physical units
!> @param[inout] physical_field3 Third component of the  output field in physical units
!> @param[in] computational_field Input field in computational units
!> @param[in] chi1 Coordinates in the first direction
!> @param[in] chi2 Coordinates in the second direction
!> @param[in] chi3 Coordinates in the third direction
!> @param[in] ndf_chi Number of degrees of freedom per cell for the coordinate field
!> @param[in] undf_chi Number of unique degrees of freedom for the coordinate field
!> @param[in] map_chi Dofmap for the cell at the base of the column for the coordinate field
!> @param[in] basis Basis functions of the output field evaluated at its nodal points
!> @param[in] diff_basis_chi Differential basis functions of the coordinate space evaluated at the nodal points
subroutine convert_hcurl_field_code(nlayers,                                  &
                                    physical_field1,                          &
                                    physical_field2,                          &
                                    physical_field3,                          &
                                    computational_field,                      &
                                    chi1, chi2, chi3,                         &
                                    ndf, undf, map,                           &
                                    ndf_chi, undf_chi, map_chi,               &
                                    basis,                                    &
                                    diff_basis_chi                            &
                                  )

end subroutine convert_hcurl_field_code

end module convert_hcurl_field_kernel_mod
