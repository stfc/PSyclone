!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details p-2020lease refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
module cross_product_mod

use constants_mod, only: r_def

implicit none

private

public :: cross_product

contains

!>@brief Function to compute the cross product of two 3D vectors x and y
!! @param[in] x The first 3d vector
!! @param[in] y The second 3d vector
!! @result z The output vector z = x cross y
pure function cross_product(x, y) result(z)

  implicit none

  real(kind=r_def), intent(in) :: x(3), y(3)
  real(kind=r_def)             :: z(3)

  z(1) = x(2)*y(3) - x(3)*y(2)
  z(2) = x(3)*y(1) - x(1)*y(3)
  z(3) = x(1)*y(2) - x(2)*y(1)
  return
end function cross_product

end module cross_product_mod

