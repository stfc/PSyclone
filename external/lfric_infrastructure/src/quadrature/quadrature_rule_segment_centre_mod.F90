!-------------------------------------------------------------------------------
! (C) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-------------------------------------------------------------------------------

!> @brief Contains the routines used for the segment centre quadrature rule.

!> @details This module contains the segment centre quadrature rule,
!! accessed via a functor from the quadrature_type. The quadrature rule locates
!! the dofs at the centres of nqp_1d equal line segments.

module quadrature_rule_segment_centre_mod

use constants_mod,       only: r_def, i_def
use quadrature_rule_mod, only: quadrature_rule_type
use matrix_invert_mod,   only: matrix_invert_lu

implicit none

private

! Public types
type, extends(quadrature_rule_type), public :: quadrature_rule_segment_centre_type
  private
contains
  procedure :: quadrature_rule
end type

! Contained functions/subroutines
contains

function quadrature_rule(self, nqp_1d)

  implicit none

  class(quadrature_rule_segment_centre_type) :: self

  integer(kind=i_def), intent(in) :: nqp_1d
  real(kind=r_def)                :: quadrature_rule(nqp_1d,2)

  integer(kind=i_def)           :: i, j
  real(kind=r_def), allocatable :: A(:,:), Ainv(:,:), b(:)

  ! quadrature_rule(:,1): points
  ! quadrature_rule(:,2): weights

  allocate( A(nqp_1d, nqp_1d),    &
            Ainv(nqp_1d, nqp_1d), &
            b(nqp_1d) )

  !-----------------------------------------------------------------------------
  ! Compute points
  !-----------------------------------------------------------------------------
  ! Let N = nqp_1d. Then the points are at the centre of N equal line segments
  ! in the interval [0,1]
  do i = 1, nqp_1d
    quadrature_rule(i, 1) = real(2*i - 1, r_def)/real(2*nqp_1d, r_def)
  end do

  !-----------------------------------------------------------------------------
  ! Compute weights
  !-----------------------------------------------------------------------------
  ! Let N = nqp_1d. The weights w_i are computed to ensure that a polynomial
  ! f(x) of degree N-1 can be integrated exactly via the relation
  !
  !   \sum_{j=1}^N w_j*f(x_j) = \int_0^1 f(x) dx
  !
  ! Since this relationship holds for any polynomial f (the weights must be
  ! independent of f), taking f(x) = x^{i-1} for 1<=i<=N yields
  !
  !   \sum_{j=1}^N  w_j*(x_j)^{i-1} = \int_0^1 x^{i-1} dx
  !                                 = 1/i
  !
  ! This is a matrix-vector problem Aw=b, where A(i,j) = (x_j)^{i-1}, w is the
  ! vector of weights, and b_i = 1/i.

  ! Construct A and b
  do i = 1, nqp_1d
    b(i) = 1.0_r_def/real(i, r_def)
    do j = 1, nqp_1d
      A(i, j) = (quadrature_rule(j, 1))**(i-1)
    end do
  end do

  ! Solve Aw=b for w
  call matrix_invert_lu(A, Ainv, nqp_1d)
  quadrature_rule(:, 2) = matmul(Ainv, b)

  return
end function quadrature_rule

end module quadrature_rule_segment_centre_mod
