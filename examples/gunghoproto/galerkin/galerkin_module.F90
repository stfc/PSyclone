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
  public galerkin_action, galerkin_action_code
  public galerkin_matrix_free_update, galerkin_matrix_free_update_code

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

  type, extends(kernel_type) :: galerkin_action
     type(arg) :: meta_args(3) = (/&
          arg(READ, (CG(1)*CG(1))**3, FE), &
          arg(INC, CG(1)*CG(1), FE), &
          arg(READ, CG(1)*CG(1), FE)/)
     integer :: ITERATES_OVER = CELLS
   contains
     procedure, nopass :: code => galerkin_action_code
  end type galerkin_action

  type, extends(kernel_type) :: galerkin_matrix_free_update
     type(arg) :: meta_args(5) = (/&
          arg(INC, (CG(1)*CG(1)), POINTWISE), &
          arg(READ, CG(1)*CG(1), POINTWISE), &
          arg(READ, CG(1)*CG(1), POINTWISE), &
          arg(READ, CG(1)*CG(1), POINTWISE), &
          arg(SUM, R, POINTWISE) &
          /)
     integer :: ITERATES_OVER = DOFS
   contains
     procedure, nopass :: code => galerkin_matrix_free_update_code
  end type galerkin_matrix_free_update

contains

  subroutine galerkin_rhs_code(layers, p1dofm, p0dofm, X, rhs, f)
    ! Evaluate rhs = v * f * dx

    integer, intent(in) :: layers
    integer, intent(in) :: p1dofm(6), p0dofm(1)
    real(dp), intent(in) :: X(3,*), f(*)
    real(dp), intent(inout) :: rhs(*)
    
    real(dp) :: J(3,3)
    integer :: k

    real(dp), parameter :: gradPhi(6,3) = reshape( &
         (/-.5, -.5, .5, .5, 0., 0., &
         -.5, -.5, 0., 0., .5, .5, &
         -1./3, 1./3, -1./3, 1./3, -1./3, 1./3/), (/6,3/))
    
    
    do k = 0, layers-1
       J=0
       
       J = matmul(X(:,p1dofm+k),gradPhi)
       
       rhs(p1dofm+k) = rhs(p1dofm+k) + f(p0dofm(1)+k)*det(J)/12.

    end do

  end subroutine galerkin_rhs_code

  subroutine galerkin_action_code(layers, p1dofm, X, out, b)
    ! Calculate out = v * b * dx for b, v in P1
    integer, intent(in) :: layers
    integer, intent(in) :: p1dofm(6)
    real(dp), intent(in) :: X(3,*)
    real(dp), intent(inout) :: out(*)
    real(dp), intent(in) :: b(*)

    ! Gradient of horizontal test functions (constant over cell). 
    ! 3 horizontal DoFs x 2 horizontal dimensions
    real(dp), parameter :: gradPhi_h(3,2) = reshape( &
         (/-1., 1., 0., &
         -1., 0., 1./), (/3,2/))
    ! Gradient of vertical test functions (constant over cell). 
    ! 2 vertical DoFs
    real(dp), parameter :: gradPhi_v(2) = (/-1., 1./)
    
    ! Horizontal basis functions evaluated at quadrature points.
    ! 4 horizontal quad points x 3 horizontal DoFs
    real(dp), parameter :: Phi_h(4, 3) = reshape( &
         (/1./3., 3./5., 1./5., 1./5., &
         1./3., 1./5., 1./5., 3./5., &
         1./3., 1./5., 3./5., 1./5./), (/4,3/))

    ! Vertical basis functions evaluated at quadrature points.
    ! 3 vertical quad points x 2 vertical DoFs.
    real(dp), parameter :: Phi_v(3,2) = reshape( &
         (/1./2., 1./2.*sqrt(3./5.) + 1./2., -1./2.*sqrt(3./5.) + 1./2., &
         1./2., -1./2.*sqrt(3./5.) + 1./2., 1./2.*sqrt(3./5.) + 1./2./), &
         (/3, 2/))

    ! Horizontal quadrature weights.
    real(dp), parameter :: w_h(4) = (/-9./32., 25./96., 25./96., 25./96./)
    ! Vertical quadrature weights.
    real(dp), parameter :: w_v(3) = (/4./9., 5./18., 5./18./)

    ! Reshape p1dofs for local convenience.
    ! 2 vertical dofs x 3 horizontal dofs.
    integer :: p1dofs(2,3) 

    ! Reshape coordinates for local convenience.
    ! 3 components x 2 vertical DoFs x 3 horizontal DoFs.
    real(dp) :: X_l(3,2,3)

    ! Jacobian
    real(dp) :: J(3,3)

    ! Loop indices
    integer :: k, q_h, q_v, i1, i2, alpha_h, alpha_v

    ! Intermediate value at quadrature point.
    real(dp) :: val_q

    p1dofs = reshape(p1dofm, (/2, 3/))

    do k = 0, layers - 1
       X_l = reshape (X(:, p1dofm+k), (/3,2,3/))
       ! Outer loop over quadrature points.
       do q_h = 1, 4
          do q_v = 1, 3
             J = 0
             ! Double loop over physical dimension for Jacobian.
             do i1 = 1, 3
                do i2 = 1, 2
                   ! Horizontal parts of Jacobian.
                   J(i1,i2) = J(i1,i2) + dot_product(&
                        matmul(X_l(i1,:,:), gradPhi_h(:,i2)), &
                        Phi_v(q_v, :))
                end do
                ! Vertical part of Jacobian.
                J(i1,3) = J(i1,3) + dot_product(&
                     matmul(X_l(i1,:,:), Phi_h(q_h,:)), gradPhi_v(:))
             end do

             val_q = 0
             ! Evaluate b at this quadrature point
             do alpha_h = 1, 3
                do alpha_v = 1, 2
                   val_q = val_q + &
                        Phi_v(q_v, alpha_v) &
                        * b(p1dofs(alpha_v, alpha_h)) &
                        * Phi_h(q_h, alpha_h)
                end do
             end do

             ! Scale by the change of coordinates and quadrature.
             val_q = val_q * det(J) * w_h(q_h) * w_v(q_v)
             
             ! Test with the various test functions.
             do alpha_h = 1, 3
                do alpha_v = 1, 2
                   out(p1dofs(alpha_v, alpha_h)) = &
                        out(p1dofs(alpha_v, alpha_h)) &
                        + Phi_v(q_v, alpha_v)*Phi_h(q_h, alpha_h) * val_q 
                end do
             end do
          end do
       end do

       ! Next layer
       p1dofs = p1dofs+1
    end do

  end subroutine galerkin_action_code

  subroutine populate_rhs_code(layers, p1dofm, p0dofm, X, f)
    ! Evaluate f = sin(z)

    integer, intent(in) :: layers
    integer, intent(in) :: p1dofm(6), p0dofm(1)
    real(dp), intent(in) :: X(3,*)
    real(dp), intent(out) :: f(*)
    
    integer :: k
    
    do k = 0, layers-1
       
       f(p0dofm(1)+k) = sin((X(3, p1dofm(2)+k) + X(3, p1dofm(1)+k))/2)

    end do

  end subroutine populate_rhs_code

  pure function det(J)
    ! Determinant of a 3x3 matrix
    real(dp), intent(in) :: J(3,3)
    real(dp) :: det

    det = abs((J(2,2)*J(3,3) - J(2,3)*J(3,2))*J(1,1) &
         - (J(1,2)*J(3,3) - J(1,3)*J(3,2))*J(2,1) &
         + (J(1,2)*J(2,3) - J(1,3)*J(2,2))*J(3,1))

  end function det

  subroutine galerkin_matrix_free_update_code(length, u, Mu, b, M_l, r)
    ! Perform the matrix-free solver update:
    ! u += (rhs-Mu)/M_l 
    real(dp), intent(inout) :: u(*)
    real(dp), intent(in) :: Mu(*), b(*), M_l(*)
    real(dp), intent(inout) :: r

    real(dp) :: res(length)

    res = b(1:length)-Mu(1:length)

    u = u + res/Ml(1:length)

    r = r + sum(res**2) 
    
  end subroutine galerkin_matrix_free_update_code


end module galerkin_module
