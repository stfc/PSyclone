!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author S. Pickles STFC Daresbury Lab

module tridiag_solve_module
  use kernel_mod
  implicit none

  private
  public tridiag_solve_kernel
  public tridiag_testdata_kernel
  public tridiag_verify_kernel
  public tridiag_solve_code
  public tridiag_testdata_code
  public tridiag_verify_code

  type, extends(kernel_type) :: tridiag_solve_kernel
     type(arg) :: meta_args(3) = (/&
          arg(READ, (DG(0)*DG(0))**3, FE), &
          arg(READ, DG(0)*DG(0), FE), &
          arg(WRITE, DG(0)*DG(0), FE)/)
     integer :: ITERATES_OVER = CELLS
   contains
     procedure, nopass :: code => tridiag_solve_code
  end type tridiag_solve_kernel

  type, extends(kernel_type) :: tridiag_testdata_kernel
     type(arg) :: meta_args(3) = (/&
          arg(READ, (CG(1)*CG(1))**3, FE), &
          arg(WRITE, (DG(0)*DG(0))**3, FE), &
          arg(WRITE, DG(0)*DG(0), FE) /)
     integer :: ITERATES_OVER = CELLS
   contains
     procedure, nopass :: code => tridiag_testdata_code
  end type tridiag_testdata_kernel

  type, extends(kernel_type) :: tridiag_verify_kernel
     type(arg) :: meta_args(4) = (/&
          arg(READ, (DG(0)*DG(0))**3, FE), &
          arg(READ, DG(0)*DG(0), FE), &
          arg(READ, DG(0)*DG(0), FE), &
          arg(SUM, R, FE)/)
     integer :: ITERATES_OVER = CELLS
   contains
     procedure, nopass :: code => tridiag_verify_code
  end type tridiag_verify_kernel

contains

  subroutine tridiag_solve_code(layers, p0dofm, T, B, X)

    integer, intent(in)     :: layers
    integer, intent(in)     :: p0dofm(1)
    real(dp), intent(in)    :: T(3,*)        ! The matrix
!
!   T(1,*), T(2,*), T(3,*) are respectively
!     the lower sub-diagonal, diagonal, and upper sub-diagonal
!
    real(dp), intent(in)    :: B(*)          ! The RHS
    real(dp), intent(out)   :: X(*)          ! The solution

    integer :: k
    real(dp) :: m

    real(dp), dimension(:), allocatable :: cp

    allocate( cp(layers) )

!   cp(1) = u(1)/d(1)
    cp(1) = T(3,0+p0dofm(1))/T(2,0+p0dofm(1))

!   x(1) = b(1)/d(1)
    x(0+p0dofm(1)) = B(0+p0dofm(1))/T(2,0+p0dofm(1))

!   do i = 2,n
    do k = 1, layers-1

!     m = 1.0d0/(d(i)-cp(i-1)*l(i-1))
!     cp(i) = u(i)*m
!     x(i) = (b(i)-x(i-1)*l(i-1))*m

      m = 1.0d0/(T(2,k+p0dofm(1))-cp(k)*T(1,k-1+p0dofm(1)))
      cp(k+1) = T(3,k+p0dofm(1))*m
      X(k+p0dofm(1)) = (B(k+p0dofm(1))-X(k-1+p0dofm(1))*T(1,k-1+p0dofm(1)))*m

    end do

!   do i = n-1, 1, -1
!     x(i) = x(i)-cp(i)*x(i+1)
!   end do
    do k = layers-2, 0, -1
      X(k+p0dofm(1)) = X(k+p0dofm(1))-cp(k+1)*X(k+1+p0dofm(1))
    end do

    deallocate(cp)
  end subroutine tridiag_solve_code

  subroutine tridiag_testdata_code(layers, cdofm, tdofm, C, T, B)

    integer, intent(in)     :: layers
    integer, intent(in)     :: cdofm(*), tdofm(*)
    real(dp), intent(in)    :: C(3,*)        ! The coordinate field
!
!   The coordinate field is passed in so that we could generate
!   location-dependent test data. We do not currently do this.
!
    real(dp), intent(out)   :: T(3,*)        ! The matrix
!
!   T(1,*), T(2,*), T(3,*) are respectively
!     the lower sub-diagonal, diagonal, and upper sub-diagonal
!
    real(dp), intent(out)   :: B(*)          ! The RHS

    integer :: k

    do k = 0, layers-2
      T(1,k+tdofm(1)) = -1.0_dp
      T(2,k+tdofm(1)) =  4.0_dp
      T(3,k+tdofm(1)) = -1.0_dp
    end do
    T(1,layers-1+tdofm(1)) =  0.0_dp
    T(2,layers-1+tdofm(1)) =  4.0_dp
    T(3,layers-1+tdofm(1)) =  0.0_dp

    do k = 0, layers-1
      B(k+tdofm(1)) = &
        (real(layers,kind=dp)+real(k,kind=dp))/real(layers,kind=dp)
    end do

  end subroutine tridiag_testdata_code

  subroutine tridiag_verify_code(layers, p0dofm, T, B, X, E)

    integer, intent(in)     :: layers
    integer, intent(in)     :: p0dofm(1)
    real(dp), intent(in)    :: T(3,*)        ! The matrix
!
!   T(1,*), T(2,*), T(3,*) are respectively
!     the lower sub-diagonal, diagonal, and upper sub-diagonal
!
    real(dp), intent(in)    :: B(*)          ! The RHS
    real(dp), intent(in)    :: X(*)          ! The candidate solution

!   This follows the integrate_one example, but
!   I am not entirely comfortable with E being array valued.
!   We could do it differently.
    real(dp), intent(inout) :: E(*)          ! cumulative error

    integer :: k
    integer :: offset
    real(dp) :: w, err

!   To make numbering look more natural in fortran
    offset = p0dofm(1)-1

!   E(1) = E(1) + 1.0_dp

!   Calculate the error |TX-B| (i.e. in the 2-norm)

    ! (TX)(1) - B(1) 
    w = T(2,1+offset)*X(1+offset) + T(3,1+offset)*X(2+offset)
    err = (w - B(1+offset))**2.0d0
    do k=2,layers-1
      ! (TX)(k) - B(k) 
      w =   T(1,k-1+offset)*X(k-1+offset)     &
          + T(2,k  +offset)*X(k  +offset)     &
          + T(3,k  +offset)*X(k+1+offset)
      err = err + (w - B(k+offset))**2.0d0
    end do
    ! (TX)(layers) - B(layers)
    w =   T(1,layers-1+offset)*X(layers-1+offset) &
        + T(2,layers  +offset)*X(layers  +offset)
    err = err + (w - B(layers+offset))**2.0d0

    err = sqrt(err)

    E(1) = E(1) + err

  end subroutine tridiag_verify_code

end module tridiag_solve_module
