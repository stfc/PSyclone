!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Provides access to the members of the v1_kernel class.

!> @details Accessor functions for the v1_kernel class are defined in this module.

!> @param RHS_v1_code              Code to implement the RHS for a v1 field
!> @param gaussian_quadrature      Contains result of gaussian quadrature

module v1_kernel_mod
use gaussian_quadrature_mod, only: gaussian_quadrature_type, &
                                   ngp_h, ngp_v ! parameter for how many GQ points
use argument_mod,            only: arg_type, &          ! the type
                                   gh_inc, v1, fe, cells ! the enums
use kernel_mod,              only: kernel_type
use constants_mod,           only: dp

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: v1_kernel_type
  private
  type(arg_type) :: meta_args(1) = (/ &
       arg_type(gh_inc,v1,fe, .true., .false., .false., .true.) &
       /)
  integer :: iterates_over = cells

contains
  procedure, nopass :: rhs_v1_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface v1_kernel_type
   module procedure v1_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public rhs_v1_code              
contains

type(v1_kernel_type) function v1_kernel_constructor() result(self)
  return
end function v1_kernel_constructor
  
subroutine rhs_v1_code(nlayers,ndf,map,v1_basis,x,gq)

!> @brief This subroutine calculates the RHS of Galerkin projection on W1 space.
!! @param[in] nlayers Integer: The number of layers.
!! @param[in] ndf Integer: The number of degrees of freedom per cell.
!! @param[in] map Integer: Array holding the dofmap for the cell at the base of the column.
!! @param[in] v3_basis Real: 4-dim array holding VECTOR basis functions evaluated at quadrature points.
!! @param[inout] X Real: The array of actual data.
!! @param[inout] gq Type: Quadrature rule (here Gaussian).

  ! Needs to compute the integral of c_df * P 
  ! P_analytic over a single column
  
  !Arguments
  integer, intent(in) :: nlayers, ndf
  integer, intent(in) :: map(ndf)
  real(kind=dp), intent(in), dimension(3,ndf,ngp_h,ngp_v) :: v1_basis 
  real(kind=dp), intent(inout) :: x(*)
  type(gaussian_quadrature_type), intent(inout) :: gq

  !Internal variables
  integer               :: df, k
  integer               :: qp1, qp2
  real(kind=dp), dimension(ngp_h,ngp_v) :: f
  real(kind=dp), dimension(1,3)  :: basisfun_i
  real(kind=dp), dimension(3,1)  :: constantvec
  real(kind=dp), dimension(1,1) :: T_1

  constantvec(1,1) =  1.0_dp;
  constantvec(2,1) =  2.0_dp;  
  constantvec(3,1) =  4.0_dp;  
  ! Compute the analytic R integrated over one cell
  do k = 0, nlayers-1
    do df = 1, ndf
       do qp1 = 1, ngp_h
          do qp2 = 1, ngp_v
             basisfun_i(1,1:3) = v1_basis(1:3,df,qp1,qp2)
             T_1 = MATMUL(basisfun_i,constantvec)
             f(qp1,qp2) = T_1(1,1)
          end do
       end do
       ! Push data to global array
       x(map(df) + k) = x(map(df) + k) + gq%integrate(f) 
    end do
  end do
end subroutine rhs_v1_code

end module v1_kernel_mod
