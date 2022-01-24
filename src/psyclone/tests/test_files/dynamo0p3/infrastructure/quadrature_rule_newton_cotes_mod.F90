!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Contains the routines used for (Newton-Cotes) quadrature rule.

!> @details This module contains the (Newton-Cote) quadrature rule accessed via
!> a functor from the quadrature_type.

module quadrature_rule_newton_cotes_mod
use constants_mod, only: r_def, i_def, PI, EPS
use quadrature_rule_mod, only: quadrature_rule_type
use matrix_invert_mod, only: matrix_invert

implicit none
private

!Public types
type, extends(quadrature_rule_type), public :: quadrature_rule_newton_cotes_type
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

  class(quadrature_rule_newton_cotes_type) :: self
  integer(kind=i_def), intent(in)      :: nqp_1d
  real(kind=r_def)                     :: quadrature_rule(nqp_1d,2)

  integer :: i, j
  real(kind=r_def), allocatable :: A(:,:), Ainv(:,:), b(:)
  real(kind=r_def), parameter   :: DOMAIN_CHANGE_FACTOR = 0.5_r_def

  ! quadrature_rule(:,1) => points
  ! quadrature_rule(:,2) => weights

  allocate( A   (nqp_1d,nqp_1d),  &
            Ainv(nqp_1d,nqp_1d),  &
            b   (nqp_1d) )

  if ( nqp_1d == 1 ) then
    quadrature_rule(1,1) = 0.5_r_def
  else
    do i = 1,nqp_1d
     quadrature_rule(i,1) = real(i-1,r_def)/real(nqp_1d-1,r_def)
    end do
  end if

  ! Compute weights
  do i = 1,nqp_1d
    b(i) = (real(nqp_1d,r_def)**i-1.0_r_def)/real(i,r_def)
  end do
  ! Compute coefficient matrix A
  do i = 1,nqp_1d
    do j = 1,nqp_1d
      A(i,j) = real(j,r_def)**(i-1)
    end do
  end do
  call matrix_invert(A,Ainv,nqp_1d)
  quadrature_rule(:,2) = matmul(Ainv,b)

  quadrature_rule(:,2) = DOMAIN_CHANGE_FACTOR*quadrature_rule(:,2)

  return
end function quadrature_rule

end module quadrature_rule_newton_cotes_mod
