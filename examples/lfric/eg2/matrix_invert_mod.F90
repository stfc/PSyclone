!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

module matrix_invert_mod
use constants_mod, only: dp

implicit none

contains
!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
!> Subroutine Computes the inverse of the matrix an n x n 
!! matrix, a and returns the inverse in b.
!! uses Dolittle alogorithm for LU factorisation on a small, dense, matrix.
!! @param[in] a Real 2-dim array. Holds the values of the matrix
!! @param[out] b Real 2-dim array. Holds the values of the computed inverse
!! @param[in] n Integer. The rank/dim size of the matrix/array
subroutine matrix_invert(a,b,n)
!-------------------------------------------------------------------------------
! Compute the inverse (b) of n x n matrix a
! using LU factorization by Dolittle algorithm
!-------------------------------------------------------------------------------
integer, intent(in) :: n
real(kind=dp), intent(in)  :: a(n,n)
real(kind=dp), intent(out) :: b(n,n)

integer :: i, j, m

real(kind=dp) :: c(n,n), l(n,n), u(n,n)
real(kind=dp) :: x(n), y(n), z(n)
real(kind=dp) :: coeff

! take copy of a and initialise l, u
do i=1,n
  do j=1,n
    c(i,j) = a(i,j)
    l(i,j) = 0.0_dp
    u(i,j) = 0.0_dp
  end do
  z(i) = 0.0_dp
end do

do m=1,n-1
  do i=m+1,n
    coeff = - c(i,m)/c(m,m)    
    l(i,m) = -coeff
! Set a^m = L_m * a^m-1
    do j=m+1,n
      c(i,j) = c(i,j) + coeff*c(m,j)
    end do
  end do
end do

do i=1,n
! Set diagonal of l = 1
  l(i,i) = 1.0_dp
  do j=1,i
! u to be what is in c  
    u(j,i) = c(j,i)
  end do
end do

do m=1,n
! solve Ly = z = I by forward substitution
! since L(i,i) = 1 can remove 1/L(i,i) factor from y(i) computations
  z(m) = 1.0_dp
  y(1) = z(1)
  do i=2,n
    y(i) = z(i)
    do j=1,i-1
      y(i) = y(i) - y(j)*l(i,j)
    end do
  end do
! solve Ux = y by backward substitution
  x(n) = y(n)/u(n,n)
  do i=n-1,1,-1
    x(i) = y(i)
    do j=n,i+1,-1
      x(i) = x(i) - x(j)*u(i,j)
    end do
    x(i) = x(i)/u(i,i)
  end do
! reset z(m) to zero for next pass  
  z(m) = 0.0_dp
! fill x into column of b
  do i=1,n
    b(i,m) = x(i)
  end do
end do

end subroutine matrix_invert

end module matrix_invert_mod
