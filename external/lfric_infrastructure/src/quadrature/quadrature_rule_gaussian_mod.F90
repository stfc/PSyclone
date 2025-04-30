!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Contains the routines used for (Gaussian) quadrature rule.

!> @details This module contains the (Gaussian) quadrature rule accessed via
!> a functor from the quadrature_type.

module quadrature_rule_gaussian_mod
use constants_mod, only: r_def, i_def, PI, EPS
use quadrature_rule_mod, only: quadrature_rule_type

implicit none
private

!Public types
type, extends(quadrature_rule_type), public :: quadrature_rule_gaussian_type
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

  class(quadrature_rule_gaussian_type) :: self
  integer(kind=i_def), intent(in)      :: nqp_1d
  real(kind=r_def)                     :: quadrature_rule(nqp_1d,2)

  integer                     :: i, j, m
  real(kind=r_def)            :: p1, p2, p3, pp, z, z1
  real(kind=r_def), parameter :: DOMAIN_CHANGE_FACTOR = 0.5_r_def
  real(kind=r_def), parameter :: DOMAIN_SHIFT_FACTOR  = 1.0_r_def

  ! quadrature_rule(:,1) => points
  ! quadrature_rule(:,2) => weights

  z1 = 0.0_r_def
  m = (nqp_1d + 1) / 2

  !Roots are symmetric in the interval - so only need to find half of them

  do i = 1, m ! Loop over the desired roots

    z = cos( PI * (i - 0.25_r_def) / (nqp_1d + 0.5_r_def) )

    !Starting with the above approximation to the ith root, we enter the main
    !loop of refinement by NEWTON'S method
    pp = 1.0_r_def
    do while ( abs(z-z1) > eps )
      p1 = 1.0_r_def
      p2 = 0.0_r_def

      !Loop up the recurrence relation to get the Legendre polynomial evaluated
      !at z
      do j = 1, nqp_1d
        p3 = p2
        p2 = p1
        p1 = ((2.0_r_def * j - 1.0_r_def) * z * p2 - (j - 1.0_r_def) * p3) / j
      end do

      !p1 is now the desired Legendre polynomial. We next compute pp, its
      !derivative, by a standard relation involving also p2, the polynomial of
      ! one lower order.
      pp = nqp_1d * (z * p1 - p2)/(z*z - 1.0_r_def)
      z1 = z
      z = z1 - p1/pp             ! Newton's Method
    end do

    quadrature_rule(i,1) =  - z                                  ! Roots will be bewteen -1.0 & 1.0
    quadrature_rule(nqp_1d+1-i,1) =  + z                         ! and symmetric about the origin
    quadrature_rule(i,2) = 2.0_r_def/((1.0_r_def - z*z) * pp*pp) ! Compute the weight and its
    quadrature_rule(nqp_1d+1-i,2) = quadrature_rule(i,2)         ! symmetric counterpart

  end do ! i loop

  !Shift quad points from [-1,1] to [0,1]
  do i=1,nqp_1d
    quadrature_rule(i,1) = DOMAIN_CHANGE_FACTOR*(quadrature_rule(i,1) + DOMAIN_SHIFT_FACTOR)
    quadrature_rule(i,2) = DOMAIN_CHANGE_FACTOR*quadrature_rule(i,2)
  end do

  return
end function quadrature_rule

end module quadrature_rule_gaussian_mod
