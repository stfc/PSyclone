!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief Tangent linear for vertical reconstruction.
!> @details The nonlinear model is:
!!          \f$ F = \rho u \f$
!!          The tangent linear model is:
!!          \f$ F = \rho ls_u + ls_\rho u \f$
!!          Near the boundaries the polynomial is no longer upwinded
!!          and reduces to an extrapolation at the boundaries.
module tl_poly1d_vert_w3_reconstruction_kernel_mod

use argument_mod,      only : arg_type, func_type,         &
                              reference_element_data_type, &
                              GH_FIELD, GH_SCALAR,         &
                              GH_REAL, GH_INTEGER,         &
                              GH_LOGICAL,                  &
                              GH_INC, GH_READ, GH_BASIS,   &
                              CELL_COLUMN,                 &
                              ANY_DISCONTINUOUS_SPACE_1
use constants_mod,     only : r_def, i_def, l_def
use fs_continuity_mod, only : W2, W3
use kernel_mod,        only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the PSy layer
type, public, extends(kernel_type) :: tl_poly1d_vert_w3_reconstruction_kernel_type
  private
  type(arg_type) :: meta_args(8) = (/                                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_INC,   W2),                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W3),                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W2),                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W3),                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  ANY_DISCONTINUOUS_SPACE_1), &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ),                             &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ),                             &
       arg_type(GH_SCALAR, GH_LOGICAL, GH_READ)                              &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: tl_poly1d_vert_w3_reconstruction_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public tl_poly1d_vert_w3_reconstruction_code

contains

!> @brief Computes the tangent linear for vertical reconstructions.
!! @param[in]  nlayers      Number of layers
!! @param[in,out] reconstruction  ACTIVE Change in mass reconstruction field
!! @param[in]  tracer       ACTIVE Change in tracer
!! @param[in]  ls_wind      Linearisation state wind field
!! @param[in]  ls_tracer    Linearisation state tracer tracer
!! @param[in]  coeff        Array of polynomial coefficients for interpolation
!! @param[in]  ndata        Number of data points per dof location
!! @param[in]  global_order Desired order of polynomial reconstruction
!! @param[in]  logspace     If true perform interpolation in log space
!! @param[in]  ndf_w2       Number of degrees of freedom per cell
!! @param[in]  undf_w2      Number of unique degrees of freedom for the
!!                          reconstruction & wind fields
!! @param[in]  map_w2       Dofmap for the cell at the base of the column
!! @param[in]  ndf_w3       Number of degrees of freedom per cell
!! @param[in]  undf_w3      Number of unique degrees of freedom for tracer
!! @param[in]  map_w3       Cell dofmaps for the tracer space
!! @param[in]  ndf_c        Number of degrees of freedom per cell for the
!!                          coeff space
!! @param[in]  undf_c       Total number of degrees of freedom for the
!!                          coeff space
!! @param[in]  map_c        Dofmap for the coeff space
subroutine tl_poly1d_vert_w3_reconstruction_code(                      &
                                     nlayers,                          &
                                     reconstruction,                   &
                                     tracer,                           &
                                     ls_wind,                          &
                                     ls_tracer,                        &
                                     coeff,                            &
                                     ndata,                            &
                                     global_order,                     &
                                     logspace,                         &
                                     ndf_w2,                           &
                                     undf_w2,                          &
                                     map_w2,                           &
                                     ndf_w3,                           &
                                     undf_w3,                          &
                                     map_w3,                           &
                                     ndf_c,                            &
                                     undf_c,                           &
                                     map_c )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)                    :: nlayers
  integer(kind=i_def), intent(in)                    :: ndata
  integer(kind=i_def), intent(in)                    :: ndf_w3
  integer(kind=i_def), intent(in)                    :: undf_w3
  integer(kind=i_def), intent(in)                    :: ndf_w2
  integer(kind=i_def), intent(in)                    :: undf_w2
  integer(kind=i_def), intent(in)                    :: ndf_c
  integer(kind=i_def), intent(in)                    :: undf_c
  integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
  integer(kind=i_def), dimension(ndf_c),  intent(in) :: map_c
  integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
  integer(kind=i_def), intent(in)                    :: global_order
  real(kind=r_def), dimension(undf_w2), intent(out)  :: reconstruction
  real(kind=r_def), dimension(undf_w3), intent(in)   :: tracer
  real(kind=r_def), dimension(undf_w2), intent(in)   :: ls_wind
  real(kind=r_def), dimension(undf_w3), intent(in)   :: ls_tracer
  real(kind=r_def), dimension(undf_c),  intent(in)   :: coeff

  logical(kind=l_def), intent(in) :: logspace

  ! Internal variables
  integer(kind=i_def) :: k, kmin, kmax, ij, ik, p
  integer(kind=i_def) :: vertical_order, use_upwind, upwind_offset, upwind

  integer(kind=i_def), dimension(global_order+1) :: stencil

  real(kind=r_def) :: polynomial_tracer
  real(kind=r_def) :: ls_polynomial_tracer

  ! Ensure that we reduce the order if there are only a few layers
  vertical_order = min(global_order, nlayers-1)

  ! If order is even then we are using an upwind stencil -> use_upwind = 1
  ! For odd orders it is zero
  use_upwind = mod(vertical_order+1_i_def, 2_i_def)
  ij = map_w3(1)

  ! Compute dtracer/dz using precomputed weights
  do k = 0, nlayers

    ! Compute the stencil of points required
    ! For vertical_order = 2 => stencil = (k-1,k,k+1)
    ! For vertical_order = 3 => stencil = (k-2,k-1,k,k+1)
    do p = 0, vertical_order
      stencil(p+1) = k - floor(real(vertical_order+1_i_def,r_def)/2.0_r_def) + p
    end do

    ! Adjust the stencil based upon the wind sign for upwind (even order)
    ! reconstructions only.
    ! if wind > 0 -> upwind_offset = 1
    ! if wind < 0 -> upwind_offset = 0
    upwind = int(0.5_r_def*(1.0_r_def + sign(1.0_r_def,ls_wind(map_w2(5)+k))),i_def)
    upwind_offset = use_upwind*upwind

    ! Adjust the stencil in the upwind direction unless at the top boundary
    if ( k < nlayers ) stencil = stencil - upwind_offset

    ! Adjust stencil near boundaries to avoid going out of bounds
    kmin = minval(stencil(1:vertical_order+1))
    if ( kmin < 0 ) stencil = stencil - kmin
    kmax = maxval(stencil(1:vertical_order+1)) - (nlayers-1)
    if ( kmax > 0 ) stencil = stencil - kmax

    ! Compute the tracer reconstructed at W2 points
    if ( logspace ) then
      ! Linearisation state
      ! Interpolate log(tracer)
      ! I.e. polynomial = exp(c_1*log(tracer_1) + c_2*log(tracer_2) + ...)
      !                 = tracer_1**c_1*tracer_2**c_2...
      ! Note that we further take the absolute value before raising to the
      ! fractional power. This code should only be used for a positive
      ! quantity, but adding in the abs ensures no errors are thrown
      ! if negative numbers are passed through in redundant calculations
      ! in the haloes
      polynomial_tracer = 0.0_r_def
      ls_polynomial_tracer = 1.0_r_def
      do p = 1, vertical_order + 1
        ik = p + upwind_offset*(global_order+1) + k*ndata + map_c(1) - 1
        polynomial_tracer = polynomial_tracer &
                          + coeff(ik)*tracer(ij + stencil(p))/ls_tracer(ij + stencil(p))

        ls_polynomial_tracer = ls_polynomial_tracer * abs(ls_tracer(ij + stencil(p)))**coeff(ik)
      end do
      polynomial_tracer = polynomial_tracer * ls_polynomial_tracer

    else
      polynomial_tracer = 0.0_r_def
      do p = 1, vertical_order + 1
        ik = p + upwind_offset*(global_order+1) + k*ndata + map_c(1) - 1
        polynomial_tracer = polynomial_tracer + coeff(ik)*tracer(ij + stencil(p))
      end do
    end if

    reconstruction(map_w2(5)+k) = polynomial_tracer

  end do

end subroutine tl_poly1d_vert_w3_reconstruction_code

end module tl_poly1d_vert_w3_reconstruction_kernel_mod
