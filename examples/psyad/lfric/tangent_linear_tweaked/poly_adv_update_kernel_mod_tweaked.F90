!-----------------------------------------------------------------------------
! (C) Crown copyright 2022-2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which computes the horizontal advective update of a tracer
!!        where the tracer has been reconstructed on cell edges using
!!        an upwind scheme. There are two values per edge (corresponding to the
!!        two possible upwind directions) and the sign of the
!!        wind at the Wtheta point is used to dertemine the correct value to
!!        use.
module poly_adv_update_kernel_mod

use argument_mod,      only : arg_type, func_type, &
                              GH_FIELD, GH_REAL,   &
                              GH_WRITE, GH_READ,   &
                              STENCIL, CROSS2D,    &
                              CELL_COLUMN,         &
                              ANY_DISCONTINUOUS_SPACE_1
use constants_mod,     only : r_tran, i_def, l_def
use fs_continuity_mod, only : W2, Wtheta
use kernel_mod,        only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the PSy layer
type, public, extends(kernel_type) :: poly_adv_update_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                                          &
       arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, Wtheta),                                      &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  ANY_DISCONTINUOUS_SPACE_1, STENCIL(CROSS2D)), &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W2, STENCIL(CROSS2D))                         &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: poly_adv_update_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: poly_adv_update_code

contains

!> @brief Computes the horizontal advective update of a field
!> @param[in]     nlayers        Number of layers
!> @param[in,out] advective      Field containing the horizontal advective
!!                               update: (u,v).grad_h(reconstruction)
!> @param[in]     reconstruction Multidata field containing the edge
!!                               reconstruction for each cell if it is
!!                               the upwind cell for that edge
!> @param[in]     smap_md_size   Size of the multidata stencil map in
!!                               each direction
!> @param[in]     smap_md_max    Maximum size of the multidata stencil map
!> @param[in]     smap_md        Stencil map for the multidata fields
!> @param[in]     wind           Wind field
!> @param[in]     smap_w2_size   Size of the w2stencil map in each direction
!> @param[in]     smap_w2_max    Maximum size of the w2 stencil map
!> @param[in]     smap_w2        Stencil map for the w2 fields
!> @param[in]     ndf_wt         Number of degrees of freedom per cell
!> @param[in]     undf_wt        Number of unique degrees of freedom for the advective field
!> @param[in]     map_wt         Dofmap for the cell at the base of the column
!> @param[in]     ndf_md         Number of degrees of freedom per cell
!> @param[in]     undf_md        Number of unique degrees of freedom for the
!!                               reconstructed field
!> @param[in]     map_md         Dofmap for the cell at the base of the column
!> @param[in]     ndf_w2         Number of degrees of freedom per cell
!> @param[in]     undf_w2        Number of unique degrees of freedom for the wind field
!> @param[in]     map_w2         Dofmap for the cell at the base of the column
subroutine poly_adv_update_code( nlayers,                &
                                 advective,              &
                                 reconstruction,         &
                                 smap_md_size,           &
                                 smap_md_max,            &
                                 smap_md,                &
                                 wind,                   &
                                 smap_w2_size,           &
                                 smap_w2_max,            &
                                 smap_w2,                &
                                 ndf_wt,                 &
                                 undf_wt,                &
                                 map_wt,                 &
                                 ndf_md,                 &
                                 undf_md,                &
                                 map_md,                 &
                                 ndf_w2,                 &
                                 undf_w2,                &
                                 map_w2 )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)                    :: nlayers
  integer(kind=i_def), intent(in)                    :: ndf_wt
  integer(kind=i_def), intent(in)                    :: undf_wt
  integer(kind=i_def), dimension(ndf_wt), intent(in) :: map_wt
  integer(kind=i_def), intent(in)                    :: ndf_md
  integer(kind=i_def), intent(in)                    :: undf_md
  integer(kind=i_def), dimension(ndf_md), intent(in) :: map_md
  integer(kind=i_def), intent(in)                    :: ndf_w2
  integer(kind=i_def), intent(in)                    :: undf_w2
  integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2

  integer(kind=i_def),                                    intent(in) :: smap_md_max
  integer(kind=i_def), dimension(4),                      intent(in) :: smap_md_size
  integer(kind=i_def), dimension(ndf_md, smap_md_max, 4), intent(in) :: smap_md
  integer(kind=i_def),                                    intent(in) :: smap_w2_max
  integer(kind=i_def), dimension(4),                      intent(in) :: smap_w2_size
  integer(kind=i_def), dimension(ndf_w2, smap_w2_max, 4), intent(in) :: smap_w2

  real(kind=r_tran), dimension(undf_wt), intent(inout) :: advective
  real(kind=r_tran), dimension(undf_md), intent(in)    :: reconstruction
  real(kind=r_tran), dimension(undf_w2), intent(in)    :: wind

  ! Internal variables
  integer(kind=i_def)                      :: k, df, ijp, df1, df2
  integer(kind=i_def), dimension(4)        :: direction_dofs
  integer(kind=i_def), parameter           :: nfaces = 4
  real(kind=r_tran)                         :: direction
  real(kind=r_tran), dimension(nfaces)      :: v_dot_n
  real(kind=r_tran), dimension(4,0:nlayers) :: tracer
  real(kind=r_tran), dimension(2,0:nlayers) :: uv
  real(kind=r_tran)                         :: dtdx, dtdy

  integer(kind=i_def), dimension(nfaces) :: opposite
  logical(kind=l_def), dimension(nfaces) :: missing_neighbour
  integer(kind=i_def), parameter :: W = 1
  integer(kind=i_def), parameter :: S = 2
  integer(kind=i_def), parameter :: E = 3
  integer(kind=i_def), parameter :: N = 4

  ! Causes an error in the SymPy parser.
  !v_dot_n = (/ -1.0_r_tran, 1.0_r_tran, 1.0_r_tran, -1.0_r_tran /)
  v_dot_n(:) = 1.0_r_tran
  v_dot_n(1) = -1.0_r_tran
  v_dot_n(4) = -1.0_r_tran
  
  ! For each face of cell, find the index in the neighbouring cell that
  ! corresponds to it.
  ! i.e for no orientation changes opposite = ( 3, 4, 1, 2 )
  ! We use the W2 map to determine these
  ! If there is no neighbour then we ensure the opposite points to
  ! the value on this edge
  opposite = -1
  missing_neighbour = .false.
  do df = 1,nfaces
    df1 = map_w2(df)
    if ( smap_w2_size(df) > 1 ) then
      ! There is a neighbour in direction df so find the
      ! neighboring edge corresponding to edge df
      do df2 = 1, nfaces
        if ( smap_w2(df2,2,df) == df1 ) opposite(df) = df2
      end do
    else
      ! There is no neighbour in direction df so point to itself
      opposite(df) = df
      missing_neighbour(df) = .true.
    end if
  end do

  k = 0
  uv(1,k) = 0.25_r_tran*( wind(map_w2(1)) + wind(map_w2(3)) )
  uv(2,k) = 0.25_r_tran*( wind(map_w2(2)) + wind(map_w2(4)) )
  do k = 1, nlayers-1
    uv(1,k) = 0.25_r_tran*( wind(map_w2(1) + k - 1 ) + wind(map_w2(1) + k ) &
                         + wind(map_w2(3) + k - 1 ) + wind(map_w2(3) + k ) )
    uv(2,k) = 0.25_r_tran*( wind(map_w2(2) + k - 1 ) + wind(map_w2(2) + k ) &
                         + wind(map_w2(4) + k - 1 ) + wind(map_w2(4) + k ) )
  end do
  k = nlayers
  uv(1,k) = 0.25_r_tran*( wind(map_w2(1) + k - 1) + wind(map_w2(3) + k - 1) )
  uv(2,k) = 0.25_r_tran*( wind(map_w2(2) + k - 1) + wind(map_w2(4) + k - 1) )

  ! Horizontal advective update
  ! Reconstruction is stored on a layer first multidata field
  ! with index for face f = (1,2,3,4) = (W,S,E,N): map_md(1) + (f-1)*(nlayers+1) + k
  ! each cell contains the values for when it is the upwind cell for each edge
  ! so if u.n > 0 then we set the field to be the value on this edge from this cell
  ! and if u.n < 0 then we set the field to be the value on this edge from a
  ! neighbouring cell
  ! Causes error in SymPy writer.
  !direction_dofs = (/ 1, 2, 1, 2 /)
  direction_dofs(1:,2) = 1
  direction_dofs(2:,2) = 2
  do df = 1,nfaces
    do k = 0, nlayers
      direction = uv(direction_dofs(df),k)*v_dot_n(df)
      if ( direction > 0.0_r_tran .or. missing_neighbour(df) ) then
        ! Take value on edge from this column
        ijp = map_md(1) + (df-1)*(nlayers+1)
        tracer(df, k) = reconstruction(ijp + k )
      else
        ! Take value on edge from neighbouring column
        ijp = smap_md(1, 2, df) + (opposite(df)-1)*(nlayers+1)
        tracer(df, k ) = reconstruction(ijp + k )
      end if
    end do
  end do

  do k = 0, nlayers
    dtdx = tracer(E, k) - tracer(W, k)
    dtdy = tracer(N, k) - tracer(S, k)
    advective(map_wt(1)+k) = uv(1,k)*dtdx - uv(2,k)*dtdy
  end do

end subroutine poly_adv_update_code

end module poly_adv_update_kernel_mod
