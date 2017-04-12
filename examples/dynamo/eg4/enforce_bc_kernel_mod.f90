!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which applies boundary conditions to a field
!> @detail Wrapper code for applying boundary conditions to a field
!>         When the Psyclone api is updated to correctly deal with
!>         boundary dofs this can be removed
module enforce_bc_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_INC,                        &
                                    ANY_SPACE_1,                             &
                                    CELLS
use constants_mod,           only : r_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: enforce_bc_kernel_type
  private
  type(arg_type) :: meta_args(1) = (/                                  &
       arg_type(GH_FIELD,   GH_INC,  ANY_SPACE_1)                      &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::enforce_bc_code
end type

!-------------------------------------------------------------------------------
! Constenforce_bcctors
!-------------------------------------------------------------------------------

! overload the default stenforce_bccture constenforce_bcctor for function space
interface enforce_bc_kernel_type
   module procedure enforce_bc_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public enforce_bc_code
contains

type(enforce_bc_kernel_type) function enforce_bc_kernel_constructor() result(self)
  return
end function enforce_bc_kernel_constructor

!> @brief The subroutine which is called directly by the Psy layer
!! @param[in] nlayers Integer the number of layers
!! @param[in] ndf The number of degrees of freedom per cell
!! @param[in] undf The number unique of degrees of freedom
!! @param[in] map Integer array holding the dofmap for the cell at the base of the column
!! @param[in] boundary_value array of flags (= 0) for dofs that live on the
!!            vertical boundaries of the cell (=1 for other dofs)
!! @param[inout] field Real array the data

subroutine enforce_bc_code(nlayers,                        &
                           field,                          &
                           ndf, undf, map, boundary_value  &
                          )
 
  !Arguments
  integer, intent(in) :: nlayers
  integer, intent(in) :: ndf
  integer, intent(in) :: undf
  integer, dimension(ndf),   intent(in) :: map
  integer, dimension(ndf,2), intent(in) :: boundary_value

  real(kind=r_def), dimension(undf), intent(inout) :: field

  ! Local variables
  integer :: df, k

  k = 0
  do df = 1,ndf
    field(map(df) + k) = field(map(df) + k)*real(boundary_value(df,1))
  end do
  k = nlayers - 1 
  do df = 1,ndf
    field(map(df) + k) = field(map(df) + k)*real(boundary_value(df,2))
  end do

end subroutine enforce_bc_code

end module enforce_bc_kernel_mod
