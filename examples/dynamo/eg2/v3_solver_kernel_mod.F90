!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which computes LHS of Galerkin projection and solves equation

module v3_solver_kernel_mod
use kernel_mod, only : kernel_type
use constants_mod, only : dp
use gaussian_quadrature_mod, only : ngp_h, ngp_v, gaussian_quadrature_type
use argument_mod,            only : arg_type, &          ! the type
                                    gh_read, gh_write, v3, fe, cells ! the enums

use matrix_invert_mod,       only : matrix_invert                                 
implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: v3_solver_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/ &
       arg_type(gh_write,v3,fe,.true.,.false.,.false.,.true.),        &
       arg_type(gh_read ,v3,fe,.false.,.false.,.false.,.false.)       &
       /)
  integer :: iterates_over = cells
contains
  procedure, nopass ::solver_v3_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface v3_solver_kernel_type
   module procedure v3_solver_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public solver_v3_code
contains

type(v3_solver_kernel_type) function v3_solver_kernel_constructor() result(self)
  return
end function v3_solver_kernel_constructor

!> @brief The subroutine which is called directly by the Psy layer
!! @param[in] nlayers Integer the number of layers
!! @param[in] ndf The number of degrees of freedom per cell
!! @param[in] map Integer array holding the dofmap for the cell at the base of the column
!!  @param[in] v3_basis Real 5-dim array holding basis functions evaluated at gaussian quadrature points 
!! @param[inout] X Real array the data 
!! @param[in] rhs Real array. the data
!! @param[inout] gq The gaussian quadrature rule 
subroutine solver_v3_code(nlayers,ndf,map,v3_basis,x,rhs,gq)
  ! needs to compute the integral of rho_df * P 
  ! P_analytic over a single column
  
  !Arguments
  integer, intent(in) :: nlayers, ndf
  integer, intent(in) :: map(ndf)
!  real(kind=dp), intent(in), dimension(ndf,ngp,ngp,ngp,1) :: v3_basis  
  real(kind=dp), intent(in), dimension(1,ndf,ngp_h,ngp_v) :: v3_basis  
  real(kind=dp), intent(inout) :: x(*)
  real(kind=dp), intent(in) :: rhs(*)
  type(gaussian_quadrature_type), intent(inout) :: gq

  !Internal variables
  integer               :: df1, df2, k
  integer               :: qp1, qp2
  
  real(kind=dp) :: x_e(ndf), rhs_e(ndf)
  real(kind=dp), dimension(ngp_h,ngp_v) :: f
  real(kind=dp), dimension(ndf,ndf) :: mass_matrix_v3, inv_mass_matrix_v3
  
  ! compute the LHS integrated over one cell and solve
  do k = 0, nlayers-1
    do df1 = 1, ndf
       do df2 = 1, ndf
          do qp1 = 1, ngp_h
             do qp2 = 1, ngp_v
                f(qp1,qp2) = v3_basis(1,df1,qp1,qp2) * &
                             v3_basis(1,df2,qp1,qp2)
                end do
             end do
             mass_matrix_v3(df1,df2) = gq%integrate(f)
          end do
          rhs_e(df1) = rhs(map(df1)+k)
       end do
       call matrix_invert(mass_matrix_v3,inv_mass_matrix_v3,ndf)
       x_e = matmul(inv_mass_matrix_v3,rhs_e)
       do df1 = 1,ndf
          x(map(df1)+k) = x_e(df1) 
       end do
    end do
  
  end subroutine solver_v3_code

end module v3_solver_kernel_mod
