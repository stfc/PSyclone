!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

module matrix_vector_kernel_mod
use argument_mod,            only : arg_type,                               &
                                    GH_FIELD, GH_OPERATOR, GH_READ, GH_INC, &
                                    ANY_SPACE_1, ANY_SPACE_2,               &
                                    CELLS 
use constants_mod,           only : r_def
use kernel_mod,              only : kernel_type

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: matrix_vector_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                  &
       arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1),                    &  
       arg_type(GH_FIELD,    GH_READ, ANY_SPACE_2),                    &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2)        &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::matrix_vector_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface matrix_vector_kernel_type
   module procedure matrix_vector_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public matrix_vector_code
contains

  type(matrix_vector_kernel_type) function matrix_vector_kernel_constructor() result(self)
  return
end function matrix_vector_kernel_constructor

!> @brief The subroutine which is called directly by the Psy layer, computes lhs = matrix*x
!> @param[in] cell the horizontal cell index
!! @param[in] nlayers Integer the number of layers
!! @param[in] ncell_3d total number of cells
!! @param[in] ndf1 The number of degrees of freedom per cell for the output field
!! @param[in] undf1 The unique number of degrees of freedom  for the output field
!! @param[in] map1 Integer array holding the dofmap for the cell at the base of the column for the output field
!! @param[in] map2 Integer array holding the dofmap for the cell at the base of the column for the input field
!! @param[in] ndf2 The number of degrees of freedom per cell for the input field
!! @param[in] undf2 The unique number of degrees of freedom for the input field 
!! @param[in] x Real array the data
!> @param[inout] lhs Real array, the output lhs (A*x)
!! @param[in] mass_matrix Real: Array holding mass matrix values
subroutine matrix_vector_code(cell,        &
                              nlayers,     &
                              lhs, x,      & 
                              ncell_3d,    &
                              matrix,      &
                              ndf1, undf1, map1, &
                              ndf2, undf2, map2)
 
  !Arguments
  integer,                   intent(in)    :: cell, nlayers, ncell_3d
  integer,                   intent(in)    :: undf1, ndf1
  integer,                   intent(in)    :: undf2, ndf2
  integer, dimension(ndf1),  intent(in)    :: map1
  integer, dimension(ndf2),  intent(in)    :: map2
  real(kind=r_def), dimension(undf2),              intent(in)    :: x
  real(kind=r_def), dimension(undf1),              intent(inout) :: lhs
  real(kind=r_def), dimension(ndf1,ndf2,ncell_3d), intent(in)    :: matrix

  !Internal variables
  integer                           :: df, k, ik 
  real(kind=r_def), dimension(ndf2) :: x_e
  real(kind=r_def), dimension(ndf1) :: lhs_e
 
  do k = 0, nlayers-1
    do df = 1, ndf2  
      x_e(df) = x(map2(df)+k)
    end do
    ik = (cell-1)*nlayers + k + 1
    lhs_e = matmul(matrix(:,:,ik),x_e)
    do df = 1,ndf1
       lhs(map1(df)+k) = lhs(map1(df)+k) + lhs_e(df) 
    end do
  end do
 
end subroutine matrix_vector_code

end module matrix_vector_kernel_mod
