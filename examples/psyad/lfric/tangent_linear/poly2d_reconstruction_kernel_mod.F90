!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which computes horizontal tracer values through fitting a
!!        high order 2D upwind reconstruction.
!> @details Computes the reconstruction for a tracer field using a high order
!!          polynomial fit to the integrated tracer values. The stencil used
!!          for the polynomial is centred on the upwind cell.
!!          A 2D region stencil centred around the cell is used to build the
!!          polynomial representation of the tracer field.
!!          This method is only valid for lowest order elements.
module poly2d_reconstruction_kernel_mod

use argument_mod,      only : arg_type, func_type,       &
                              GH_FIELD, GH_SCALAR,       &
                              GH_REAL, GH_INTEGER,       &
                              GH_WRITE, GH_READ,         &
                              STENCIL, REGION,           &
                              CELL_COLUMN,               &
                              ANY_DISCONTINUOUS_SPACE_1, &
                              ANY_DISCONTINUOUS_SPACE_2, &
                              ANY_DISCONTINUOUS_SPACE_3
use constants_mod,     only : r_tran, i_def, l_def
use kernel_mod,        only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the PSy layer
type, public, extends(kernel_type) :: poly2d_reconstruction_kernel_type
  private
  type(arg_type) :: meta_args(5) = (/                                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, ANY_DISCONTINUOUS_SPACE_1), &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  ANY_DISCONTINUOUS_SPACE_2, STENCIL(REGION)), &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  ANY_DISCONTINUOUS_SPACE_3), &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ),                             &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ)                              &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: poly2d_reconstruction_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: poly2d_reconstruction_code

contains

!> @brief Computes the horizontal polynomial interpolation of a tracer.
!> @param[in]     nlayers          Number of layers
!> @param[in,out] reconstruction   Reconstructed tracer field to compute
!> @param[in]     tracer           Pointwise tracer field to reconstruct
!> @param[in]     cells_in_stencil Number of cells needed to compute the polynomial
!!                                 fit (may be less than stencil_size)
!> @param[in]     stencil_map      Dofmaps for the stencil
!> @param[in]     coeff            Array of polynomial coefficients for interpolation
!> @param[in]     ndata            Number of data points per dof location
!> @param[in]     stencil_size     Size of the stencil (number of cells)
!> @param[in]     ndf_md           Number of degrees of freedom per cell
!> @param[in]     undf_md          Number of unique degrees of freedom for the
!!                                 reconstructed field
!> @param[in]     map_md           Dofmap for the cell at the base of the column
!> @param[in]     ndf_ws           Number of degrees of freedom per cell
!> @param[in]     undf_ws          Number of unique degrees of freedom for the tracer field
!> @param[in]     map_ws           Dofmap for the cell at the base of the column for the tracer field
!> @param[in]     ndf_c            Number of degrees of freedom per cell for the coeff space
!> @param[in]     undf_c           Total number of degrees of freedom for the coeff space
!> @param[in]     map_c            Dofmap for the coeff space
subroutine poly2d_reconstruction_code( nlayers,              &
                                       reconstruction,       &
                                       tracer,               &
                                       cells_in_stencil,     &
                                       stencil_map,          &
                                       coeff,                &
                                       ndata,                &
                                       stencil_size,         &
                                       ndf_md,               &
                                       undf_md,              &
                                       map_md,               &
                                       ndf_ws,               &
                                       undf_ws,              &
                                       map_ws,               &
                                       ndf_c,                &
                                       undf_c,               &
                                       map_c )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)                    :: nlayers
  integer(kind=i_def), intent(in)                    :: ndf_ws
  integer(kind=i_def), intent(in)                    :: undf_ws
  integer(kind=i_def), dimension(ndf_ws), intent(in) :: map_ws
  integer(kind=i_def), intent(in)                    :: ndf_md
  integer(kind=i_def), intent(in)                    :: undf_md
  integer(kind=i_def), dimension(ndf_md), intent(in) :: map_md
  integer(kind=i_def), intent(in)                    :: ndf_c
  integer(kind=i_def), intent(in)                    :: undf_c
  integer(kind=i_def), dimension(ndf_c),  intent(in) :: map_c
  integer(kind=i_def), intent(in)                    :: ndata
  integer(kind=i_def), intent(in)                    :: cells_in_stencil
  integer(kind=i_def), intent(in)                    :: stencil_size

  real(kind=r_tran), dimension(undf_md), intent(inout):: reconstruction
  real(kind=r_tran), dimension(undf_ws), intent(in)   :: tracer
  real(kind=r_tran), dimension(undf_c),  intent(in)   :: coeff

  integer(kind=i_def), dimension(ndf_ws,stencil_size), intent(in) :: stencil_map


  ! Internal variables
  integer(kind=i_def) :: k, f, p, id_r, id_t, id_c, nl
  integer(kind=i_def), parameter :: nfaces = 4

  ! Number of layers to loop over:
  !  nl = nlayers - 1 for W3 fields (ndf_ws = 1)
  !  nl = nlayers     for Wt fields (ndf_ws = 2)
  nl = (nlayers - 1) + (ndf_ws - 1)

  ! Horizontal reconstruction computation
  ! field is reconstructed on a layer-first Wtheta or W3 multidata field
  ! so the index on the West  face (face=1) is: map_md(1) + 0*(nl+1) + k
  ! so the index on the South face (face=2) is: map_md(1) + 1*(nl+1) + k
  ! so the index on the East  face (face=3) is: map_md(1) + 2*(nl+1) + k
  ! so the index on the North face (face=4) is: map_md(1) + 3*(nl+1) + k
  ! i.e for face f index is map_md(1) + (f-1)*(nl+1) + k
  do f = 1,nfaces
    id_r = map_md(1) + (f-1)*(nl+1)
    do k = 0, nl
      reconstruction(id_r + k) = 0.0_r_tran
    end do
    do p = 1,cells_in_stencil
      id_c = (p - 1 + (f-1)*stencil_size) + map_c(1)
      id_t = stencil_map(1,p)
      do k = 0, nl
        reconstruction(id_r + k) = reconstruction(id_r + k) &
                                 + tracer( id_t + k )*coeff( id_c )
      end do
    end do
  end do

end subroutine poly2d_reconstruction_code

end module poly2d_reconstruction_kernel_mod
