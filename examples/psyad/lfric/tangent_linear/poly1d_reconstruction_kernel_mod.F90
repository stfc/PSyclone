!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which computes horizontal tracer values through fitting a high
!!        order 1D upwind reconstruction.
!> @details Computes the reconstruction for a tracer field using a high order
!!          polynomial fit to the integrated tracer values. The stencil used
!!          for the polynomial is centred on the upwind cell.
!!          A 1D cross stencil centred around the cell is used to build the
!!          polynomial representation of the tracer field.
!!          This method is only valid for lowest order elements.
module poly1d_reconstruction_kernel_mod

use argument_mod,      only : arg_type, func_type,         &
                              reference_element_data_type, &
                              GH_FIELD, GH_SCALAR,         &
                              GH_REAL, GH_INTEGER,         &
                              GH_WRITE, GH_READ,           &
                              STENCIL, CROSS,              &
                              CELL_COLUMN,                 &
                              ANY_DISCONTINUOUS_SPACE_1,   &
                              ANY_DISCONTINUOUS_SPACE_2,   &
                              ANY_DISCONTINUOUS_SPACE_3
use constants_mod,     only : r_tran, i_def
use kernel_mod,        only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the PSy layer
type, public, extends(kernel_type) :: poly1d_reconstruction_kernel_type
  private
  type(arg_type) :: meta_args(5) = (/                                                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, ANY_DISCONTINUOUS_SPACE_1 ),                &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  ANY_DISCONTINUOUS_SPACE_2, STENCIL(CROSS)), &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  ANY_DISCONTINUOUS_SPACE_3),                 &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ),                                             &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ)                                              &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: poly1d_reconstruction_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: poly1d_reconstruction_code

contains

!> @brief Computes the horizontal polynomial interpolation of a tracer.
!> @param[in]     nlayers        Number of layers
!> @param[in,out] reconstruction Reconstructed tracer field to compute
!> @param[in]     tracer         Pointwise tracer field to reconstruct
!> @param[in]     stencil_size   Size of the stencil (number of cells)
!> @param[in]     stencil_map    Dofmaps for the stencil
!> @param[in]     coeff          Array of polynomial coefficients for interpolation
!> @param[in]     ndata          Number of data points per dof location
!> @param[in]     order          Desired order of polynomial reconstruction
!> @param[in]     ndf_md         Number of degrees of freedom per cell for reconstructed field
!> @param[in]     undf_md        Number of unique degrees of freedom for the
!!                               reconstructed field
!> @param[in]     map_md         Dofmap for the cell at the base of the column for reconstructed field
!> @param[in]     ndf_ws         Number of degrees of freedom per cell for the tracer
!> @param[in]     undf_ws        Number of unique degrees of freedom for the tracer field
!> @param[in]     map_ws         Dofmap for the cell at the base of the column for the tracer field
!> @param[in]     ndf_c          Number of degrees of freedom per cell for the coeff space
!> @param[in]     undf_c         Total number of degrees of freedom for the coeff space
!> @param[in]     map_c          Dofmap for the coeff space
subroutine poly1d_reconstruction_code( nlayers,        &
                                       reconstruction, &
                                       tracer,         &
                                       stencil_size,   &
                                       stencil_map,    &
                                       coeff,          &
                                       ndata,          &
                                       order,          &
                                       ndf_md,         &
                                       undf_md,        &
                                       map_md,         &
                                       ndf_ws,         &
                                       undf_ws,        &
                                       map_ws,         &
                                       ndf_c,          &
                                       undf_c,         &
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
  integer(kind=i_def), intent(in)                    :: order
  integer(kind=i_def), intent(in)                    :: stencil_size

  real(kind=r_tran), dimension(undf_md), intent(inout) :: reconstruction
  real(kind=r_tran), dimension(undf_ws), intent(in)    :: tracer
  real(kind=r_tran), dimension(undf_c),  intent(in)    :: coeff

  integer(kind=i_def), dimension(ndf_ws,stencil_size), intent(in) :: stencil_map

  ! Internal variables
  integer(kind=i_def)                            :: k, f, p, face, stencil,        &
                                                    stencil_depth, depth, face_mod, &
                                                    ijp, df, nl
  integer(kind=i_def), parameter                 :: nfaces = 4
  integer(kind=i_def), dimension(order+1,nfaces) :: map1d

  ! Number of layers to loop over:
  !  nl = nlayers - 1 for W3 fields (ndf_ws = 1)
  !  nl = nlayers     for Wt fields (ndf_ws = 2)
  nl = (nlayers - 1) + (ndf_ws - 1)

  ! Compute 1d map from the cross stencil
  ! i.e for order = 2 the stencil map is
  !      | 5 |
  !  | 2 | 1 | 4 |
  !      | 3 |
  ! so map1d is
  ! ( 1, 2, 4 )
  ! ( 1, 3, 5 )
  ! ( 1, 2, 4 )
  ! ( 1, 3, 5 )
  ! First cell is always the centre cell
  stencil_depth = order/2
  map1d(1,:) = 1
  do face = 1,nfaces
    depth=1
    face_mod = mod(face+1,2) * stencil_depth
    do stencil = 2,stencil_depth+1
      map1d(stencil+depth-1, face) = stencil + face_mod
      map1d(stencil+depth, face) = stencil + order + face_mod
      depth=depth+1
    end do
  end do

  ! Horizontal reconstruction computation
  ! field is reconstructed on a layer-first Wtheta or W3 multidata field
  ! so the index on the West  face (face=1) is: map_md(1) + 0*(nl+1) + k
  ! so the index on the South face (face=2) is: map_md(1) + 1*(nl+1) + k
  ! so the index on the East  face (face=3) is: map_md(1) + 2*(nl+1) + k
  ! so the index on the North face (face=4) is: map_md(1) + 3*(nl+1) + k
  ! i.e for face f index is map_md(1) + (f-1)*(nl+1) + k
  do f = 1,nfaces
    df = map_md(1) + (f-1)*(nl+1)
    do k = 0, nl
      reconstruction(df + k) = 0.0_r_tran
    end do
    do p = 1, order+1
      ijp = (p - 1 + (f-1)*(order+1)) + map_c(1)
      stencil = map1d(p,f)
      do k = 0, nl
        reconstruction(df + k) = reconstruction(df + k) &
                               + tracer( stencil_map(1,stencil) + k )*coeff( ijp )
      end do
    end do

  end do

end subroutine poly1d_reconstruction_code

end module poly1d_reconstruction_kernel_mod
