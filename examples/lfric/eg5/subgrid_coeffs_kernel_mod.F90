!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2020, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
!------------------------------------------------------------------------------
! Modified by I. Kavcic, Met Office

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

use argument_mod,       only : arg_type, GH_FIELD, &
                               GH_READ, GH_WRITE,  &
                               STENCIL, CROSS, CELLS
use fs_continuity_mod,  only : W3
use constants_mod,      only : r_def, i_def, l_def
use subgrid_config_mod, only : subgrid_rho_approximation_constant_subgrid,     &
                               subgrid_rho_approximation_constant_positive,    &
                               subgrid_rho_approximation_linear_centered_diff, &
                               subgrid_rho_approximation_linear_superbee,      &
                               subgrid_rho_approximation_linear_minmod,        &
                               subgrid_rho_approximation_ppm_no_limiter,       &
                               subgrid_rho_approximation_ppm_positive_only,    &
                               subgrid_rho_approximation_ppm_positive_monotone,&
                               rho_stencil_length
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
       arg_type(GH_FIELD, GH_WRITE, W3),                               &
       arg_type(GH_FIELD, GH_WRITE, W3),                               &
       arg_type(GH_FIELD, GH_WRITE, W3),                               &
       arg_type(GH_FIELD, GH_READ,  W3, STENCIL(CROSS))                &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: subgrid_coeffs_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public subgrid_coeffs_code

contains

!> @brief The subroutine which is called directly by the Psy layer
!! @param[in] nlayers Integer the number of layers
!! @param[in] subgridrho_option Integer Option for which approximation to use
!! @param[in] undf_w3 Integer The number unique of degrees of freedom for W3
!! @param[in] rho Real array The density
!! @param[in] stencil_length Integer The local length of a stencil (5 for PPM)
!! @param[in] local_dofmap Integer array Local array containg dofmaps of the local stencil
!! @param[out] coeffs Real array Array containing the three coefficients, a0,a1,a2
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

  use subgrid_rho_mod, only: return_ppm_output, minmod_function, maxmod_function, subgridrho_option


  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers
  integer(kind=i_def), intent(in) :: undf_w3
  real(kind=r_def), intent(in)    :: rho(undf_w3)
  integer(kind=i_def), intent(in) :: ndf_w3
  integer(kind=i_def), intent(in) :: stencil_length
  integer(kind=i_def), intent(in) :: stencil_map(1:ndf_w3,1:stencil_length)
  integer(kind=i_def), intent(in) :: w3_map()
  real(kind=r_def), intent(out)   :: a0(undf_w3)
  real(kind=r_def), intent(out)   :: a1(undf_w3)
  real(kind=r_def), intent(out)   :: a2(undf_w3)

  real(kind=r_def)               :: sigma1,sigma2
  real(kind=r_def)               :: coeffs(1:3)

  integer(kind=i_def) :: k

  logical(kind=l_def) :: positive, monotone

  do k=0,nlayers-1

    select case(subgridrho_option)
      case (subgrid_rho_approximation_constant_subgrid)
        a0(stencil_map(1,1)) = rho(stencil_map(1,1))
        a1(stencil_map(1,1)) = 0.0_r_def
        a2(stencil_map(1,1)) = 0.0_r_def

      case (subgrid_rho_approximation_constant_positive)
        a0(stencil_map(1,1)) = max(rho(stencil_map(1,1)),0.0_r_def)
        a1(stencil_map(1,1)) = 0.0_r_def
        a2(stencil_map(1,1)) = 0.0_r_def

      case (subgrid_rho_approximation_linear_centered_diff)
        a1(stencil_map(1,1)) = (rho(stencil_map(1,3))-rho(stencil_map(1,2)))/2.0_r_def
        a0(stencil_map(1,1)) = rho(stencil_map(1,1))-a1(stencil_map(1,1))*0.5_r_def
        a2(stencil_map(1,1)) = 0.0_r_def

      case (subgrid_rho_approximation_linear_superbee)
        sigma1 = minmod_function(                                             &
                      rho(stencil_map(1,3))-rho(stencil_map(1,1)),            &
                      2.0_r_def*(rho(stencil_map(1,1))-rho(stencil_map(1,2))) &
                                )
        sigma2 = minmod_function(                                             &
                      2.0_r_def*(rho(stencil_map(1,3))-rho(stencil_map(1,1))),&
                      rho(stencil_map(1,1))-rho(stencil_map(1,2)) )

        a1(stencil_map(1,1)) = maxmod_function( sigma1, sigma2)
        a0(stencil_map(1,1)) = rho(stencil_map(1,1))-a1(stencil_map(1,1))*0.5_r_def
        a2(stencil_map(1,1)) = 0.0_r_def

      case (subgrid_rho_approximation_linear_minmod)
        a1(stencil_map(1,1)) = minmod_function(                               &
                                rho(stencil_map(1,1))-rho(stencil_map(1,2)) , &
                                rho(stencil_map(1,3))-rho(stencil_map(1,1)) )
        a0(stencil_map(1,1)) = rho(stencil_map(1,1))-a1(stencil_map(1,1))*0.5_r_def
        a2(stencil_map(1,1)) = 0.0_r_def

      case (subgrid_rho_approximation_ppm_no_limiter)
        positive=.false.
        monotone=.false.
        call return_ppm_output(rho(stencil_map(1,1:5)),coeffs,positive,monotone)
        a0(stencil_map(1,1)) = coeffs(1)
        a1(stencil_map(1,1)) = coeffs(2)
        a2(stencil_map(1,1)) = coeffs(3)

      case (subgrid_rho_approximation_ppm_positive_only, &
            subgrid_rho_approximation_ppm_positive_monotone)
        positive=.true.
        monotone=.false.
        if ( subgridrho_option == subgrid_rho_approximation_ppm_positive_monotone) monotone=.true.
        call return_ppm_output(rho(stencil_map(1,1:5)),coeffs,positive,monotone)
        a0(stencil_map(1,1)) = coeffs(1)
        a1(stencil_map(1,1)) = coeffs(2)
        a2(stencil_map(1,1)) = coeffs(3)

    end select

  end do

end subroutine subgrid_coeffs_code

end module subgrid_coeffs_kernel_mod
