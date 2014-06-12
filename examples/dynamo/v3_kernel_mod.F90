!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which operates on v3 field. Determines the RHS of Galerkin projection

!> @detail The kernel computes the integral of rho_df * P 
!! with P_analytic over a single column


module v3_kernel_mod
use lfric
use argument_mod,            only: arg_type, &          ! the type
                                   gh_rw, v3, fe, cells ! the enums


implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: v3_kernel_type
  private
  type(arg_type) :: meta_args(1) = (/ &
       arg_type(gh_rw,v3,fe,.true.,.false.,.true.) &
       /)
  integer :: iterates_over = cells

contains
  procedure, nopass :: rhs_v3_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface v3_kernel_type
   module procedure v3_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public rhs_v3_code              
contains

type(v3_kernel_type) function v3_kernel_constructor() result(self)
  return
end function v3_kernel_constructor

!> @brief The subroutine which is called directly by the psy layer
!! @param[in] nlayers Integer the number of layers
!! @param[in] ndf The number of degrees of freedom per cell
!! @param[in] map Integer array holding the dofmap for the cell at the base of the column
!! @param[in] v3_basis Real 5-dim array holding basis functions evaluated at gaussian quadrature points
!! @param[inout] X Real array, the actual data
!! @param[inout] gq Type, gaussian quadrature rule
subroutine rhs_v3_code(nlayers,ndf,map,v3_basis,x,gq)
  ! needs to compute the integral of rho_df * P 
  ! P_analytic over a single column
  
  !Arguments
  integer, intent(in) :: nlayers, ndf
  integer, intent(in) :: map(ndf)
  real(kind=dp), intent(in), dimension(ndf,ngp,ngp,ngp,1) :: v3_basis 
  real(kind=dp), intent(inout) :: x(*)
  type(gaussian_quadrature_type), intent(inout) :: gq

  !Internal variables
  integer               :: df, k
  integer               :: qp1, qp2, qp3
  real(kind=dp), dimension(ngp,ngp,ngp) :: f
   
  ! compute the analytic R integrated over one cell
  do k = 0, nlayers-1
    do df = 1, ndf
       do qp1 = 1, ngp
          do qp2 = 1, ngp
             do qp3 = 1, ngp
                f(qp1,qp2,qp3) = v3_basis(df,qp1,qp2,qp3,1) * real(k+1)
             end do
          end do
       end do
       x(map(df) + k) = gq%integrate(f)
    end do
  end do
  
end subroutine rhs_v3_code

end module v3_kernel_mod


