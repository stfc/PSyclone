!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!> @brief Contains routines for evaluating 1D polynomials and their derivatives
module polynomial_mod

use constants_mod, only: r_def, i_def

implicit none

public

contains

!----------------------------------------------------------------------------
! Evaluate 1D basis functions of arbitary order at the points of quadrature
!----------------------------------------------------------------------------
!> Function computes the value of a arbritrary order polynomial at a given point
!> @param[in] order Order of the polynomial
!> @param[in] xi  Point to evaluate the polynomial at
!> @param[in] x Coordinate array
!> @param[in] i Index of x at which P(x(i)) = 1
pure function poly1d(order, xi, x, i)
  implicit none
  ! order of the basis function
  integer(kind=i_def), intent(in) :: order
  ! quadrature point to evaluate basis function at
  real(kind=r_def),    intent(in) :: xi
  ! nodal points
  real(kind=r_def),    intent(in) :: x(order+2)
  ! Index of basis function
  integer(kind=i_def), intent(in) :: i

  real(kind=r_def) :: poly1d
  ! internal tempories
  ! loop counters
  integer(kind=i_def)       :: j

  poly1d = 1.0_r_def

  do j = 1,i-1
     poly1d = poly1d*(xi-x(j))/(x(i)-x(j))
  end do
  do j = i+1,order+1
     poly1d = poly1d*(xi-x(j))/(x(i)-x(j))
  end do

  return
end function poly1d

!-----------------------------------------------------------------------------
! evaluate derivative of 1D basis function of arbitrary order at xk
!-----------------------------------------------------------------------------
!> Function computes the value of the derivative of a arbritrary order polynomial at a given point
!> @param[in] order Order of the polynomial
!> @param[in] xi  Point to evaluate the polynomial at
!> @param[in] x Coordinate array
!> @param[in] i Index of x at which P(x(i)) = 1
pure function poly1d_deriv(order,xi,x,i)

  implicit none
  ! Order of basis function
  integer(kind=i_def), intent(in) :: order
  ! Index of basis function
  integer(kind=i_def), intent(in) :: i
  ! Quadrature point to evaluate basis function at
  real(kind=r_def),    intent(in) :: xi
  ! Nodal points
  real(kind=r_def),    intent(in) :: x(order+2)

  ! Tempories
  real(kind=r_def)    :: poly1d_deriv
  real(kind=r_def)    :: denom,t
  integer(kind=i_def) :: k,j

  poly1d_deriv = 0.0_r_def
  denom = 1.0_r_def

  do j = 1,i-1
    denom = denom * 1.0_r_def/(x(i)-x(j))
  end do
  do j = i+1,order+1
    denom = denom * 1.0_r_def/(x(i)-x(j))
  end do

  do k = 1,i-1
    t = 1.0_r_def
    do j = 1,order+1
      if (j /= i .and. j /= k ) then
        t = t * (xi - x(j))
      end if
    end do
    poly1d_deriv = poly1d_deriv + t*denom
  end do
  do k = i+1,order+1
    t = 1.0_r_def
    do j = 1,order+1
      if (j /= i .and. j /= k ) then
        t = t * (xi - x(j))
      end if
    end do
    poly1d_deriv = poly1d_deriv + t*denom
  end do
  return
end function poly1d_deriv

end module polynomial_mod
