!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Provides access to the members of the W2_solver_kernel  

!> @details Accessor functions for the W2_solver_kernel class are defined in this module.

module matrix_vector_mm_mod
use argument_mod,            only : arg_type,                               &
                                    GH_FIELD, GH_OPERATOR, GH_READ, GH_INC, &
                                    ANY_SPACE_1,                            &
                                    CELLS 
use constants_mod,           only : r_def
use kernel_mod,              only : kernel_type

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: matrix_vector_kernel_mm_type
  private
  type(arg_type) :: meta_args(3) = (/                                  &
       arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1),                    &  
       arg_type(GH_FIELD,    GH_READ, ANY_SPACE_1),                    &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_1)        &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::matrix_vector_mm_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface matrix_vector_kernel_mm_type
   module procedure matrix_vector_kernel_mm_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public matrix_vector_mm_code
contains

  type(matrix_vector_kernel_mm_type) function matrix_vector_kernel_mm_constructor() result(self)
  return
end function matrix_vector_kernel_mm_constructor

!> @brief The subroutine which is called directly by the Psy layer, computes mass_matrix*x
!> @param[in]  cell the horizontal cell index
!! @param[in] nlayers Integer the number of layers
!! @param[in] ndf The number of degrees of freedom per cell
!! @param[in] undf The unique number of degrees of freedom 
!! @param[in] map Integer array holding the dofmap for the cell at the base of the column
!! @param[in] x Real array the data
!> @param[inout] lhs Real array, the output lhs (A*x)
!! @param[in] ncell_3d total number of cells
!! @param[in] mass_matrix Real: Array holding mass matrix values
subroutine matrix_vector_mm_code(cell,        &
                                 nlayers,     &
                                 lhs, x,      & 
                                 ncell_3d,    &
                                 mass_matrix, &
                                 ndf,undf,map)
 
  !Arguments
  integer,                   intent(in)    :: cell, nlayers, ndf
  integer,                   intent(in)    :: undf, ncell_3d
  integer, dimension(ndf),   intent(in)    :: map
  real(kind=r_def), dimension(undf), intent(in)    :: x
  real(kind=r_def), dimension(undf), intent(inout) :: lhs
  real(kind=r_def), dimension(ndf,ndf,ncell_3d), intent(in) :: mass_matrix

  !Internal variables
  integer                                  :: df, k, ik 
  real(kind=r_def), dimension(ndf)         :: x_e, lhs_e
 
  do k = 0, nlayers-1
    do df = 1, ndf  
      x_e(df) = x(map(df)+k)
    end do
    ik = (cell-1)*nlayers + k + 1
    lhs_e = matmul(mass_matrix(:,:,ik),x_e)
    do df = 1,ndf
       lhs(map(df)+k) = lhs(map(df)+k) + lhs_e(df) 
    end do
  end do
 
end subroutine matrix_vector_mm_code

end module matrix_vector_mm_mod
