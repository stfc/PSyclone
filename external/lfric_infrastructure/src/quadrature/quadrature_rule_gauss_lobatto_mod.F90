!-----------------------------------------------------------------------------
! (c) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Contains the routines used for (Gaussian-Lobatto) quadrature rule.

!> @details This module contains the (Gaussian-Lobbato) quadrature rule accessed via
!> a functor from the quadrature_type.

module quadrature_rule_gauss_lobatto_mod
use constants_mod,       only: r_def, i_def, PI, EPS
use quadrature_rule_mod, only: quadrature_rule_type

implicit none
private

!Public types
type, extends(quadrature_rule_type), public :: quadrature_rule_gauss_lobatto_type
  private
contains
  procedure :: quadrature_rule
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

function quadrature_rule(self, nqp_1d)
  implicit none

  class(quadrature_rule_gauss_lobatto_type) :: self
  integer(kind=i_def), intent(in)      :: nqp_1d
  real(kind=r_def)                     :: quadrature_rule(nqp_1d,2)

  integer(kind=i_def)                        :: i, j
  real(kind=r_def), dimension(nqp_1d)        :: x, x_old
  real(kind=r_def), dimension(nqp_1d,nqp_1d) :: p
  real(kind=r_def), parameter :: DOMAIN_CHANGE_FACTOR = 0.5_r_def
  real(kind=r_def), parameter :: DOMAIN_SHIFT_FACTOR  = 1.0_r_def

  ! quadrature_rule(:,1) => points
  ! quadrature_rule(:,2) => weights

  ! Compute the n-point Gauss-Legendre-Lobatto rule
  ! The endpoints of the domain [0,1] are included
  ! the remaining points are the roots of P'n-1(x) where
  ! P(x) is a Legendre polynomial
  ! These points are computed using a Newton method
  ! and the Legendre polynomials are computed by the recursion relation:
  ! (n+1)*P_n+1(x) = (2n+1)*x*P_n(x) - n*P_n-1(x)
  ! The Legendre-Gauss Vandermonde Matrix P_i,j contains the Legendre
  ! polynomial P_j(x_i)

  ! The weights are given by w = 2/(n*(n-1)*P^2)

  ! First guess of the points
  do i = 1,nqp_1d
     x(i) = cos(pi*real(nqp_1d-i,r_def)/real(nqp_1d-1,r_def))
   end do
   p(:,:) = 0.0_r_def
   x_old(:) = 2.0_r_def

   ! Newton loop
   do while( maxval(abs(x(:)-x_old(:))) > EPS )
     x_old = x
     ! Compute first two Legendre polynomials on all points
     p(:,1) = 1.0_r_def
     p(:,2) = x(:)

     ! Compute i+1 Legendre polynomial P_i+1 via recursion relation using P_i
     ! and P_i-1
     do i = 2,nqp_1d-1
       do j = 1,nqp_1d
         p(j,i+1) = (real(2*i-1)*x(j)*p(j,i)-real(i-1)*p(j,i-1))/real(i)
       end do
     end do
     ! Update points
     do j = 1,nqp_1d
       x(j) = x_old(j) - (x(j)*p(j,nqp_1d) - p(j,nqp_1d-1)) &
                         /(real(nqp_1d,r_def)*p(j,nqp_1d))
     end do
   end do
   do j=1,nqp_1d
     quadrature_rule(j,1) = x(j)
     quadrature_rule(j,2) = 2.0_r_def/(real((nqp_1d-1)*nqp_1d,r_def)*p(j,nqp_1d)**2)
   end do

   !Shift quad points from [-1,1] to [0,1]
   do i=1,nqp_1d
     quadrature_rule(i,1) = DOMAIN_CHANGE_FACTOR*(quadrature_rule(i,1) + DOMAIN_SHIFT_FACTOR)
     quadrature_rule(i,2) = DOMAIN_CHANGE_FACTOR*quadrature_rule(i,2)
   end do

   return
end function quadrature_rule

end module quadrature_rule_gauss_lobatto_mod
