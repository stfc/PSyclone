!-----------------------------------------------------------------------------
! (c) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which computes horizontal advective update through fitting a high order
!!        upwind reconstruction.
!> @details Compute the advective update (u.grad) for a tracer field using a high order
!!          polynomial fit to the integrated tracer values. The stencil used for the
!!          polynomial is centred on the upwind cell.
!!          This method is only valid for lowest order elements.

module w3_advective_update_kernel_mod

use argument_mod,      only : arg_type,          &
                              GH_FIELD, GH_REAL, &
                              GH_OPERATOR,       &
                              GH_WRITE, GH_READ, &
                              CELL_COLUMN
use constants_mod,     only : r_def, i_def
use fs_continuity_mod, only : W3, W2
use kernel_mod,        only : kernel_type

implicit none

private
!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: w3_advective_update_kernel_type
  private
  type(arg_type) :: meta_args(4) = (/                    &
       arg_type(GH_FIELD,    GH_REAL, GH_WRITE, W3),     &
       arg_type(GH_FIELD,    GH_REAL, GH_READ,  W2),     &
       arg_type(GH_FIELD,    GH_REAL, GH_READ,  W2),     &
       arg_type(GH_OPERATOR, GH_REAL, GH_READ,  W3, W3)  &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: w3_advective_update_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: w3_advective_update_code
contains

!> @brief Computes the horizontal advective update for a tracer in W3.
!> @param[in]     cell                Horizontal cell index
!> @param[in]     nlayers             Number of layers
!> @param[in,out] advective_increment Advective update field to compute
!> @param[in]     tracer              Pointwise tracer field to advect stored on cell faces
!> @param[in]     wind                Wind field
!> @param[in]     ncell_3d            Total number of cells
!> @param[in]     m3_inv              Inverse mass matrix for W3 space
!> @param[in]     ndf_w3              Number of degrees of freedom per cell
!> @param[in]     undf_w3             Number of unique degrees of freedom for the
!!                                    advective_update field
!> @param[in]     map_w3              Dofmap for the cell at the base of the column
!> @param[in]     ndf_w2              Number of degrees of freedom per cell for the wind fields
!> @param[in]     undf_w2             Number of unique degrees of freedom for the wind fields
!> @param[in]     map_w2              Dofmap for the cell at the base of the column for the wind fields
subroutine w3_advective_update_code( cell,                 &
                                     nlayers,              &
                                     advective_increment,  &
                                     tracer,               &
                                     wind,                 &
                                     ncell_3d,             &
                                     m3_inv,               &
                                     ndf_w3,               &
                                     undf_w3,              &
                                     map_w3,               &
                                     ndf_w2,               &
                                     undf_w2,              &
                                     map_w2                &
                                     )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)                    :: nlayers, cell, ncell_3d
  integer(kind=i_def), intent(in)                    :: ndf_w3, ndf_w2
  integer(kind=i_def), intent(in)                    :: undf_w3, undf_w2
  integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
  integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2

  real(kind=r_def), dimension(undf_w3), intent(inout) :: advective_increment
  real(kind=r_def), dimension(undf_w2), intent(in)    :: wind
  real(kind=r_def), dimension(undf_w2), intent(in)    :: tracer

  real(kind=r_def), dimension(ndf_w3, ndf_w3, ncell_3d), intent(in) :: m3_inv

  ! Internal variables
  integer(kind=i_def) :: k, ik
  real(kind=r_def)    :: u, v, w, dtdx, dtdy, dtdz

  do k = 0, nlayers - 1
    u =  0.5_r_def*( wind(map_w2(1) + k) + wind(map_w2(3) + k) )
    v = -0.5_r_def*( wind(map_w2(2) + k) + wind(map_w2(4) + k) )
    w =  0.5_r_def*( wind(map_w2(5) + k) + wind(map_w2(6) + k) )
    dtdx = tracer(map_w2(3) + k) - tracer(map_w2(1) + k)
    dtdy = tracer(map_w2(4) + k) - tracer(map_w2(2) + k)
    dtdz = tracer(map_w2(6) + k) - tracer(map_w2(5) + k)
    ik = 1 + k + (cell-1)*nlayers
    advective_increment(map_w3(1)+k) = m3_inv(1,1,ik)*(u*dtdx + v*dtdy + w*dtdz)
  end do

end subroutine w3_advective_update_code

end module w3_advective_update_kernel_mod
