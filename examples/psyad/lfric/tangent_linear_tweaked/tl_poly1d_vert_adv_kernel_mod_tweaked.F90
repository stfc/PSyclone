!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Tangent linear for computing vertical fluxes through fitting a
!!        high order 1D upwind reconstruction.

! This tweaked version of the code 1) splits the real and floor
! intrinsics onto their own lines and 2) replaces minval and maxval
! with equialent inline code.

module tl_poly1d_vert_adv_kernel_mod

use argument_mod,         only : arg_type, CELL_COLUMN, &
                                 GH_FIELD, GH_SCALAR,   &
                                 GH_REAL, GH_INTEGER,   &
                                 GH_LOGICAL,            &
                                 GH_READWRITE, GH_READ, &
                                 ANY_DISCONTINUOUS_SPACE_1
use constants_mod,        only : r_def, i_def, l_def
use fs_continuity_mod,    only : W2, Wtheta
use kernel_mod,           only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the PSy layer
type, public, extends(kernel_type) :: tl_poly1d_vert_adv_kernel_type
  private
  type(arg_type) :: meta_args(9) = (/                                            &
       arg_type(GH_FIELD,  GH_REAL,    GH_READWRITE, Wtheta),                    &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,      W2),                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,      Wtheta),                    &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,      W2),                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,      Wtheta),                    &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,      ANY_DISCONTINUOUS_SPACE_1), &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ),                                 &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ),                                 &
       arg_type(GH_SCALAR, GH_LOGICAL, GH_READ)                                  &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: tl_poly1d_vert_adv_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: tl_poly1d_vert_adv_code

contains

!> @brief Computes the tangent linear for vertical fluxes for a tracer density.
!! @param[in]  nlayers   Number of layers
!! @param[in,out] advective ACTIVE Advective update to increment
!! @param[in]  wind      ACTIVE Change in wind field
!! @param[in]  tracer    ACTIVE Change in tracer field to advect
!! @param[in]  ls_wind   Lin state for wind field
!! @param[in]  ls_tracer Lin state for tracer field to advect
!! @param[in]  coeff     Array of polynomial coefficients for interpolation
!! @param[in]  ndata     Number of data points per dof location
!! @param[in]  global_order Desired order of polynomial reconstruction
!! @param[in]  logspace  If true then perform interpolation in log space
!! @param[in]  ndf_wt    Number of degrees of freedom per cell
!! @param[in]  undf_wt   Number of unique degrees of freedom for the tracer
!!                       field
!! @param[in]  map_wt    Cell dofmaps for the tracer space
!! @param[in]  ndf_w2    Number of degrees of freedom per cell
!! @param[in]  undf_w2   Number of unique degrees of freedom for the flux &
!!                       wind fields
!! @param[in]  map_w2    Dofmap for the cell at the base of the column
!! @param[in]  ndf_c     Number of degrees of freedom per cell for the coeff
!!                       space
!! @param[in]  undf_c    Total number of degrees of freedom for the coeff space
!! @param[in]  map_c     Dofmap for the coeff space
subroutine tl_poly1d_vert_adv_code( nlayers,              &
                                    advective,            &
                                    wind,                 &
                                    tracer,               &
                                    ls_wind,              &
                                    ls_tracer,            &
                                    coeff,                &
                                    ndata,                &
                                    global_order,         &
                                    logspace,             &
                                    ndf_wt,               &
                                    undf_wt,              &
                                    map_wt,               &
                                    ndf_w2,               &
                                    undf_w2,              &
                                    map_w2,               &
                                    ndf_c,                &
                                    undf_c,               &
                                    map_c)

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)                    :: nlayers
  integer(kind=i_def), intent(in)                    :: ndf_wt
  integer(kind=i_def), intent(in)                    :: undf_wt
  integer(kind=i_def), intent(in)                    :: ndf_w2
  integer(kind=i_def), intent(in)                    :: undf_w2
  integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
  integer(kind=i_def), dimension(ndf_wt), intent(in) :: map_wt
  integer(kind=i_def), intent(in)                    :: ndf_c
  integer(kind=i_def), intent(in)                    :: undf_c
  integer(kind=i_def), dimension(ndf_c),  intent(in) :: map_c
  integer(kind=i_def), intent(in)                    :: ndata
  integer(kind=i_def), intent(in)                    :: global_order

  real(kind=r_def), dimension(undf_wt), intent(inout) :: advective
  real(kind=r_def), dimension(undf_w2), intent(in)    :: wind
  real(kind=r_def), dimension(undf_wt), intent(in)    :: tracer
  real(kind=r_def), dimension(undf_w2), intent(in)    :: ls_wind
  real(kind=r_def), dimension(undf_wt), intent(in)    :: ls_tracer
  real(kind=r_def), dimension(undf_c),  intent(in)    :: coeff

  logical(kind=l_def), intent(in) :: logspace

  ! Internal variables

  integer(kind=i_def) :: k, kmin, kmax, ij, ik, p
  integer(kind=i_def) :: vertical_order, use_upwind, upwind_offset, upwind

  integer(kind=i_def), dimension(global_order+1) :: stencil

  real(kind=r_def) :: dpdz, ls_dpdz
  real(kind=r_def), dimension(0:nlayers) :: ls_log_tracer

  real(kind=r_def) :: tmp1, tmp2, tmp3
  integer(kind=i_def) :: i

  ij = map_wt(1)

  ! For logspace the nonlinear term is:
  ! dp_{j}/dz = p_j * sum_i a_i * log( p_i)
  ! The tl term is then:
  ! dp_{j}/dz = ls_p_j * sum_i a_i * p_i / ls_p_i !
  !           + p_j * sum_i a_i * log( ls_p_i )

  ! Compute log of tracer. This code should only be used for a positive
  ! quantity, but adding in the abs ensures no errors are thrown
  ! if negative numbers are passed through in redundant calculations
  ! in the haloes
  if ( logspace ) then
    do k = 0, nlayers
      ls_log_tracer(k) = log(abs(ls_tracer(ij+k)))
    end do
  end if

  ! Ensure that we reduce the order if there are only a few layers
  vertical_order = min(global_order, nlayers-1)

  ! If order is odd then we are using an upwind stencil -> use_upwind = 1
  ! For even orders it is zero
  use_upwind = mod(vertical_order, 2_i_def)

  ! Compute dtracer/dz using precomputed weights
  do k = 1, nlayers - 1

    ! Compute the stencil of points required
    do p = 0, vertical_order
      tmp1 = real(vertical_order,r_def)
      tmp2 = tmp1/2.0_r_def
      tmp3 = floor(tmp2)
      stencil(p+1) = k - tmp3 + p
    end do

    ! Adjust the stencil based upon the wind sign for upwind (odd order)
    ! reconstructions only.
    ! if wind > 0 -> upwind_offset = 1
    ! if wind < 0 -> upwind_offset = 0
    upwind = int(0.5_r_def*(1.0_r_def + sign(1.0_r_def,ls_wind(map_w2(5)+k))),i_def)
    upwind_offset = use_upwind*upwind
    stencil = stencil - upwind_offset

    ! Adjust stencil near boundaries to avoid going out of bounds
    kmin = stencil(1)
    do i = 2, vertical_order+1
       if (stencil(i) < kmin) then
          kmin = stencil(i)
       end if
    end do
    if ( kmin < 0 ) stencil = stencil - kmin

    kmax = stencil(1)
    do i = 2, vertical_order+1
       if (stencil(i) > kmax) then
          kmax = stencil(i)
       end if
    end do
    kmax = kmax - nlayers
    if ( kmax > 0 ) stencil = stencil - kmax

    ! Compute the derivative and the advective update
    dpdz = 0.0_r_def
    ls_dpdz = 0.0_r_def
    if ( logspace ) then
      ! dp/dz = p * d(log(p))/dz
      do p = 1, vertical_order + 1
        ik = p + upwind_offset*(global_order+1) + k*ndata + map_c(1) - 1
        dpdz = dpdz + coeff(ik)*tracer(ij + stencil(p))/ls_tracer(ij + stencil(p))
        ls_dpdz = ls_dpdz + coeff(ik)*ls_log_tracer(stencil(p))
      end do
      dpdz = ls_tracer(ij + k)*dpdz + tracer(ij + k)*ls_dpdz
      ls_dpdz = ls_tracer(ij + k)*ls_dpdz
    else
      do p = 1, vertical_order + 1
        ik = p + upwind_offset*(global_order+1) + k*ndata + map_c(1) - 1
        dpdz = dpdz + coeff(ik)*tracer(ij + stencil(p))
        ls_dpdz = ls_dpdz + coeff(ik)*ls_tracer(ij + stencil(p))
      end do
    end if

    advective(map_wt(1)+ k) = advective(map_wt(1)+k) &
                              + wind(map_w2(5)+k)*ls_dpdz &
                              + ls_wind(map_w2(5)+k)*dpdz
  end do
end subroutine tl_poly1d_vert_adv_code

end module tl_poly1d_vert_adv_kernel_mod
