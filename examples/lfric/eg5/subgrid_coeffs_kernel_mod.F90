!-----------------------------------------------------------------------------
! Original under:
! Copyright (c) 2017-2026, Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! Modifications under:
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> @brief Calculates the coefficients, a0,a1,a2, for 1D subgrid
!>        representation of rho, rho(x) = a0 + a1*x+a2*x**2 with 0<x<1,
!>        here x simply represents a local coordinate within a cell in either
!>        the chi1, chi2, or chi3 directions.

!> @detail The kernel computes the coefficients a0,a1,a2 where rho is represented in 1D
!>         by the approximation rho(x) = a0+a1*x+a2*x**2
!>         Various cases for calculating a0,a1 and a2 are available, including
!>         constant,linear and quadratic subgrid representations of rho.
!>         For linear representation there are several options. If no slope limiter is
!>         required then centered difference is used to estimate the slope.
!>         Slope limiters which are currently available are minmod and superbee.
!>         These slope limiters are extensively covered in the literature on slope limiters
!>         and have good performance.
!>         For quadratic representation of rho PPM is used and the options of
!>         positivity and monotonicity are available
!>
!>         Note that this kernel only works when rho is a W3 field at lowest order
!>         since it is assumed that ndf_w3 = 1 with stencil_map(1,:) containing
!>         the relevant dofmaps.
module subgrid_coeffs_kernel_mod

use argument_mod,       only : arg_type,          &
                               GH_FIELD, GH_REAL, &
                               GH_READ, GH_WRITE, &
                               STENCIL, CROSS, CELL_COLUMN
use fs_continuity_mod,  only : W3
use constants_mod,      only : r_def, i_def, l_def
use kernel_mod,         only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: subgrid_coeffs_kernel_type
  private
  type(arg_type) :: meta_args(4) = (/                                  &
       arg_type(GH_FIELD, GH_REAL, GH_WRITE, W3),                      &
       arg_type(GH_FIELD, GH_REAL, GH_WRITE, W3),                      &
       arg_type(GH_FIELD, GH_REAL, GH_WRITE, W3),                      &
       arg_type(GH_FIELD, GH_REAL, GH_READ,  W3, STENCIL(CROSS))       &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: subgrid_coeffs_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public subgrid_coeffs_code

contains

!> @brief Compute the subgrid reconstruction coefficients for a density field
!! @param[in] nlayers Number of layers
!! @param[out] a0 Coefficient a0
!! @param[out] a1 Coefficient a1
!! @param[out] a2 Coefficient a2
!! @param[in] rho Density
!! @param[in] stencil_length Local length of a stencil (5 for PPM)
!! @param[in] stencil_map Dofmap for the stencil
!! @param[in] ndf_w3 Number of degrees of freedom for W3 per cell
!! @param[in] undf_w3 Number of unique degrees of freedom for W3
!! @param[in] map_w3 Dofmap for the cell at the base of the column for W3
subroutine subgrid_coeffs_code(                                               &
                                nlayers,                                      &
                                a0,                                           &
                                a1,                                           &
                                a2,                                           &
                                rho,                                          &
                                stencil_length,                               &
                                stencil_map,                                  &
                                ndf_w3,                                       &
                                undf_w3,                                      &
                                map_w3                                        &
                                )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers
  integer(kind=i_def), intent(in) :: undf_w3
  real(kind=r_def), intent(in)    :: rho(undf_w3)
  integer(kind=i_def), intent(in) :: ndf_w3
  integer(kind=i_def), intent(in) :: stencil_length
  integer(kind=i_def), intent(in) :: stencil_map(1:ndf_w3,1:stencil_length)
  integer(kind=i_def), intent(in) :: map_w3(ndf_w3)
  real(kind=r_def), intent(inout) :: a0(undf_w3)
  real(kind=r_def), intent(inout) :: a1(undf_w3)
  real(kind=r_def), intent(inout) :: a2(undf_w3)

  real(kind=r_def)               :: sigma1,sigma2
  real(kind=r_def)               :: coeffs(1:3)

  integer(kind=i_def) :: k

  logical(kind=l_def) :: positive, monotone

  do k=0,nlayers-1

     a0(stencil_map(1,1)) = rho(stencil_map(1,1))
     a1(stencil_map(1,1)) = 0.0_r_def
     a2(stencil_map(1,1)) = 0.0_r_def

  end do

end subroutine subgrid_coeffs_code

end module subgrid_coeffs_kernel_mod
