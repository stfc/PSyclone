!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author D. Ham Imperial College

! Example integrating the unit function over the unit cube.

module galerkin_module
  use kernel_mod
  implicit none

  private
  public galerkin_rhs_kernel, populate_rhs
  public galerkin_rhs_code, populate_rhs_code

  type, extends(kernel_type) :: galerkin_rhs_kernel
     type(arg) :: meta_args(3) = (/&
          arg(READ, (CG(1)*CG(1))**3, FE), &
          arg(INC, CG(1)*CG(1), FE), &
          arg(READ, DG(0)*DG(0), FE)/)
     integer :: ITERATES_OVER = CELLS
   contains
     procedure, nopass :: code => galerkin_rhs_code
  end type galerkin_rhs_kernel

  type, extends(kernel_type) :: populate_rhs
     type(arg) :: meta_args(2) = (/&
          arg(READ, (CG(1)*CG(1))**3, FE), &
          arg(WRITE, DG(0)*DG(0), FE)/)
     integer :: ITERATES_OVER = CELLS
   contains
     procedure, nopass :: code => populate_rhs_code
  end type populate_rhs

contains

  subroutine galerkin_rhs_code(layers, p1dofm, p0dofm, X, rhs, f)
    ! Evaluate rhs = v * f * dx

    integer, intent(in) :: layers
    integer, intent(in) :: p1dofm(6), p0dofm(1)
    real(dp), intent(in) :: X(3,*), f(*)
    real(dp), intent(inout) :: rhs(*)
    
    real(dp) :: J(3,3), detJ
    integer :: k

    real(dp), parameter :: gradPhi(6,3) = reshape( &
         [-.5, -.5, .5, .5, 0., 0., &
         -.5, -.5, 0., 0., .5, .5, &
         -1./3, 1./3, -1./3, 1./3, -1./3, 1./3], [6,3])
    
    
    do k = 0, layers-1
       J=0
       
       J = matmul(X(:,p1dofm+k),gradPhi)
       detJ = abs((J(2,2)*J(3,3) - J(2,3)*J(3,2))*J(1,1) &
            - (J(1,2)*J(3,3) - J(1,3)*J(3,2))*J(2,1) &
            + (J(1,2)*J(2,3) - J(1,3)*J(2,2))*J(3,1))
       
       rhs(p1dofm+k) = rhs(p1dofm+k) + f(p0dofm(1)+k)*detJ/12.

    end do

  end subroutine galerkin_rhs_code

  subroutine populate_rhs_code(layers, p1dofm, p0dofm, X, f)
    ! Evaluate f = sin(z)

    integer, intent(in) :: layers
    integer, intent(in) :: p1dofm(6), p0dofm(1)
    real(dp), intent(in) :: X(3,*)
    real(dp), intent(out) :: f(*)
    
    real(dp) :: J(3,3), detJ
    integer :: k

    
    do k = 0, layers-1
       
       f(p0dofm(1)+k) = sin((X(3, p1dofm(2)+k) + X(3, p1dofm(1)+k))/2)

    end do

  end subroutine populate_rhs_code

end module galerkin_module
