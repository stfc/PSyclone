!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Stores the diagonal elements of a mass matrix

!> @details Stores the diagonal elements of a mass matrix M into a field D
!>          i.e D(df) = M(df,df)


module mm_diagonal_kernel_mod
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

type, public, extends(kernel_type) :: mm_diagonal_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/                                  &
       arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1),                    &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_1)        &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::mm_diagonal_kernel_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface mm_diagonal_kernel_type
  module procedure mm_diagonal_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public mm_diagonal_kernel_code
contains

  type(mm_diagonal_kernel_type) function mm_diagonal_kernel_constructor() result(self)
  return
end function mm_diagonal_kernel_constructor

!> @brief The subroutine which is called directly by the Psy layer, strores the diagonal of a mass_matrix
!> @param[in]  cell the horizontal cell index
!! @param[in] nlayers Integer the number of layers
!! @param[in] ndf The number of degrees of freedom per cell
!! @param[in] undf The unique number of degrees of freedom 
!! @param[in] map Integer array holding the dofmap for the cell at the base of the column
!! @param[inout] mm_diag Real array the field array to store the diagonal entries
!!               of the mass matrix
!! @param[in] ncell_3d total number of cells
!! @param[in] mass_matrix Real: Array holding mass matrix values
subroutine mm_diagonal_kernel_code(cell,        &
                                   nlayers,     &
                                   mm_diag,     &
                                   ncell_3d,    &
                                   mass_matrix, &
                                   ndf,undf,map)
 
  !Arguments
  integer,                   intent(in)    :: cell, nlayers, ndf
  integer,                   intent(in)    :: undf, ncell_3d
  integer, dimension(ndf),   intent(in)    :: map
  real(kind=r_def), dimension(undf), intent(inout) :: mm_diag
  real(kind=r_def), dimension(ndf,ndf,ncell_3d), intent(in) :: mass_matrix

  !Internal variables
  integer :: df, k, ik
 
  do k = 0, nlayers-1
    ik = (cell-1)*nlayers + k + 1
    do df = 1,ndf
       mm_diag(map(df)+k) = mm_diag(map(df)+k) + mass_matrix(df,df,ik)
    end do
  end do
 
end subroutine mm_diagonal_kernel_code

end module mm_diagonal_kernel_mod
