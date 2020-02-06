!-------------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-------------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

!> @brief Applies boundary conditions to a field
!> @details Wrapper code for applying boundary conditions to a field
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

!> @brief Applies boundary conditions to a field
!! @param[in] nlayers Number of layers
!! @param[inout] field Input/Output data 
!! @param[in] ndf Number of degrees of freedom per cell
!! @param[in] undf Number unique of degrees of freedom
!! @param[in] map Dofmap for the cell at the base of the column
!! @param[in] boundary_value Flags (= 0) for dofs that live on the
!!            vertical boundaries of the cell (=1 for other dofs)
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
