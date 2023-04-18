!-----------------------------------------------------------------------------
! (c) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which computes horizontal reconstructiones through fitting a high order
!!        2D upwind reconstruction.
!> @details Computes the reconstruction for a tracer field using a high order
!!          polynomial fit to the integrated tracer values. The stencil used
!!          for the polynomial is centred on the upwind cell.
!!          A 2D region stencil centred around the cell is used to build the
!!          polynomial representation of the tracer field.
!!          This method is only valid for lowest order elements.

module poly2d_w3_reconstruction_kernel_mod

use argument_mod,      only : arg_type, func_type,         &
                              reference_element_data_type, &
                              GH_FIELD, GH_SCALAR,         &
                              GH_REAL, GH_INTEGER,         &
                              GH_INC, GH_READ,             &
                              STENCIL, REGION, GH_BASIS,   &
                              CELL_COLUMN, GH_EVALUATOR,   &
                              ANY_DISCONTINUOUS_SPACE_1,   &
                              outward_normals_to_horizontal_faces
use constants_mod,     only : r_def, i_def, l_def
use fs_continuity_mod, only : W2, W3
use kernel_mod,        only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the PSy layer
type, public, extends(kernel_type) :: poly2d_w3_reconstruction_kernel_type
  private
  type(arg_type) :: meta_args(6) = (/                                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_INC,   W2),                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W2),                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W3, STENCIL(REGION)),       &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  ANY_DISCONTINUOUS_SPACE_1), &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ),                             &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ)                              &
       /)
  type(func_type) :: meta_funcs(1) = (/                                   &
       func_type(W2, GH_BASIS)                                            &
       /)
  type(reference_element_data_type) :: meta_reference_element(1) = (/     &
       reference_element_data_type( outward_normals_to_horizontal_faces ) &
       /)
  integer :: gh_shape = GH_EVALUATOR
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: poly2d_w3_reconstruction_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: poly2d_w3_reconstruction_code

contains

!> @brief Computes the horizontal reconstructiones for a tracer density.
!> @param[in]     nlayers          Number of layers
!> @param[in,out] reconstruction   Reconstruced field to compute
!> @param[in]     wind             Wind field
!> @param[in]     tracer           Tracer field
!> @param[in]     coeff            Array of polynomial coefficients for interpolation
!> @param[in]     cells_in_stencil Number of cells needed to compute the polynomial
!!                                 fit (may be less than stencil_size)
!> @param[in]     stencil_map      Dofmaps for the stencil
!> @param[in]     ndata            Number of data points per dof location
!> @param[in]     stencil_size     Size of the stencil (number of cells)
!> @param[in]     ndf_w2           Number of degrees of freedom per cell
!> @param[in]     undf_w2          Number of unique degrees of freedom for the
!!                                 reconstruction and wind fields
!> @param[in]     map_w2           Dofmap for the cell at the base of the column
!> @param[in]     basis_w2         Basis function array evaluated at w2 nodes
!> @param[in]     ndf_w3           Number of degrees of freedom per cell
!> @param[in]     undf_w3          Number of unique degrees of freedom for the density field
!> @param[in]     map_w3           Dofmap for the cell at the base of the column
!!                                 for the density field
!> @param[in]     ndf_c            Number of degrees of freedom per cell for the coeff space
!> @param[in]     undf_c           Total number of degrees of freedom for the coeff space
!> @param[in]     map_c            Dofmap for the coeff space
!> @param[in]     nfaces_re_h      Number of horizontal neighbours
!> @param[in]     outward_normals_to_horizontal_faces
!!                                 Vector of normals to the reference element horizontal
!!                                 "outward faces"
subroutine poly2d_w3_reconstruction_code( nlayers,              &
                                          reconstruction,       &
                                          wind,                 &
                                          tracer,               &
                                          cells_in_stencil,     &
                                          stencil_map,          &
                                          coeff,                &
                                          ndata,                &
                                          stencil_size,         &
                                          ndf_w2,               &
                                          undf_w2,              &
                                          map_w2,               &
                                          basis_w2,             &
                                          ndf_w3,               &
                                          undf_w3,              &
                                          map_w3,               &
                                          ndf_c,                &
                                          undf_c,               &
                                          map_c,                &
                                          nfaces_re_h,          &
                                          outward_normals_to_horizontal_faces )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)                    :: nlayers
  integer(kind=i_def), intent(in)                    :: ndf_w3
  integer(kind=i_def), intent(in)                    :: undf_w3
  integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
  integer(kind=i_def), intent(in)                    :: ndf_w2
  integer(kind=i_def), intent(in)                    :: undf_w2
  integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
  integer(kind=i_def), intent(in)                    :: ndf_c
  integer(kind=i_def), intent(in)                    :: undf_c
  integer(kind=i_def), dimension(ndf_c),  intent(in) :: map_c
  integer(kind=i_def), intent(in)                    :: ndata
  integer(kind=i_def), intent(in)                    :: cells_in_stencil
  integer(kind=i_def), intent(in)                    :: stencil_size
  integer(kind=i_def), intent(in)                    :: nfaces_re_h

  real(kind=r_def), dimension(undf_w2), intent(inout) :: reconstruction
  real(kind=r_def), dimension(undf_w2), intent(in)    :: wind
  real(kind=r_def), dimension(undf_w3), intent(in)    :: tracer
  real(kind=r_def), dimension(undf_c),  intent(in)    :: coeff

  real(kind=r_def), dimension(3,ndf_w2,ndf_w2), intent(in) :: basis_w2

  integer(kind=i_def), dimension(ndf_w3,stencil_size), intent(in) :: stencil_map

  real(kind=r_def), intent(in) :: outward_normals_to_horizontal_faces(:,:)

  ! Internal variables
  integer(kind=i_def)                      :: k, df, p, ijp
  real(kind=r_def)                         :: direction
  real(kind=r_def), dimension(nfaces_re_h) :: v_dot_n
  real(kind=r_def), dimension(0:nlayers-1) :: polynomial_tracer

  do df = 1,nfaces_re_h
    v_dot_n(df) = dot_product(basis_w2(:,df,df),outward_normals_to_horizontal_faces(:,df))
  end do

  ! Horizontal reconstruction computation
  do df = 1, nfaces_re_h
    polynomial_tracer(:) = 0.0_r_def
    do p = 1,cells_in_stencil
      ijp = (p - 1 + (df-1)*stencil_size) + map_c(1)
      do k = 0, nlayers - 1
        polynomial_tracer(k) = polynomial_tracer(k) &
                             + tracer( stencil_map(1,p) + k )*coeff( ijp )
      end do
    end do
    do k = 0, nlayers - 1
      ! Check if this is the upwind cell
      direction = wind(map_w2(df) + k )*v_dot_n(df)
      if ( direction > 0.0_r_def ) reconstruction(map_w2(df) + k ) = polynomial_tracer(k)
    end do
  end do
end subroutine poly2d_w3_reconstruction_code

end module poly2d_w3_reconstruction_kernel_mod
