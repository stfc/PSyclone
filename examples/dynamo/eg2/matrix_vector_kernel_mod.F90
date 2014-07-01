!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Provides access to the members of the W1_solver_kernel and 
!! W2_solver_kernel class.

!> @details Accessor functions for the W1 and W2_solver_kernel class are defined in this module.

!> @param solver_v1_code           Code to implement the solver for a v1 field
!> @param gaussian_quadrature      Contains result of quadrature (here Gaussian)

module matrix_vector_mod
use gaussian_quadrature_mod, only : gaussian_quadrature_type,              &
                                    ngp_h,ngp_v
use argument_mod,            only : arg_type,                              &
                                    gh_read, gh_inc, v2, fe, cells 
use constants_mod,           only : dp
use kernel_mod,              only : kernel_type

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: matrix_vector_kernel_type
  private
  type(arg_type) :: meta_args(2) = [                                       &
       arg_type(gh_inc,v2,fe,.true.,.false.,.false.,.true.),             &  
       arg_type(gh_read ,v2,fe,.false.,.false.,.false.,.false.)            &
       ]
  integer :: iterates_over = cells
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
  
subroutine matrix_vector_code(nlayers,ndf,map,basis,lhs,x,gq)
  ! compute the integral of v*x
  !Arguments
  integer,                                     intent(in)    :: nlayers, ndf
  integer,                                     intent(in)    :: map(ndf)
  real(kind=dp), dimension(3,ndf,ngp_h,ngp_v), intent(in)    :: basis  
  real(kind=dp),                               intent(in)    :: x(*)
  real(kind=dp),                               intent(inout) :: lhs(*)
  type(gaussian_quadrature_type),              intent(inout) :: gq

  !Internal variables
  integer                                :: df1, df2, k
  integer                                :: qp1, qp2
  
  real(kind=dp), dimension(ndf)          :: x_e, lhs_e
  real(kind=dp), dimension(ngp_h,ngp_v)  :: f
  real(kind=dp), dimension(ndf,ndf)      :: mass_matrix
  real(kind=dp), dimension(1,3)          :: basisfun_i 
  real(kind=dp), dimension(3,1)          :: basisfun_j 
  real(kind=dp), dimension(1,1)          :: T_1

  ! compute the LHS integrated over one cell and solve
  do k = 0, nlayers-1
    do df1 = 1, ndf
       do df2 = df1, ndf  
          do qp1 = 1, ngp_h
             do qp2 = 1, ngp_v
                basisfun_i(1,:) = basis(:,df1,qp1,qp2)
                basisfun_j(:,1) = basis(:,df2,qp1,qp2)
                T_1 = matmul(basisfun_i,basisfun_j)
                f(qp1,qp2) = T_1(1,1)
             end do
          end do
          mass_matrix(df1,df2) = gq%integrate(f)
       end do
       do df2 = df1, 1, -1  
          mass_matrix(df1,df2) = mass_matrix(df2,df1)
       end do
       x_e(df1) = x(map(df1)+k)
    end do
    lhs_e = matmul(mass_matrix,x_e)
    ! push data to global array    
    do df1 = 1,ndf
       lhs(map(df1)+k) = lhs(map(df1)+k) + lhs_e(df1) 
    end do
 end do

end subroutine matrix_vector_code

end module matrix_vector_mod
