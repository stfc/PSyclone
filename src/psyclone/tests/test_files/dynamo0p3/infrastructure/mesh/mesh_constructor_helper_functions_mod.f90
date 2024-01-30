

!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

!> @brief Holds helper functions for constructing a mesh object
!>
module mesh_constructor_helper_functions_mod

use domain_size_config_mod, only : planar_domain_min_x, &
                                   planar_domain_max_x, &
                                   planar_domain_min_y, &
                                   planar_domain_max_y
use constants_mod,          only : i_def, i_native, r_def, pi, l_def
use log_mod,                only : log_event, log_scratch_space, &
                                   log_level, LOG_LEVEL_DEBUG



implicit none

private

public :: mesh_extruder,           &
          mesh_connectivity,       &
          set_domain_size,         &
          set_dz

! Declare type definitions used in this module
type, private :: coordinate_type
  real(r_def) :: x,y,z
end type coordinate_type

type, public :: domain_size_type
  type(coordinate_type) :: minimum, maximum
  real(r_def) :: base_height
end type domain_size_type

contains

  !===========================================================================
  !> @brief Helper function to extrude the surface 2D-mesh into the 3D-mesh.
  !>
  !> (called from the mesh constructor)
  !>
  !> @param[out] cell_next           Cell IDs of adjacent cells.
  !> @param[out] vert_on_cell        Vertex IDs on cell.
  !> @param[out] vertex_coords       Vertex Coordinates.
  !> @param[in]  nfaces              Number of faces per 3D cell.
  !> @param[in]  nverts              Number of vertices per 3D cell.
  !> @param[in]  cell_next_2d        List of neighbouring cells for each 2D &
  !>                                 cell.
  !> @param[in]  vert_on_cell_2d     List of vertices on each 2D cell.
  !> @param[in]  vertex_coords_2d    Local 2d cell connectivity.
  !> @param[in]  is_spherical     True: vertex_coords_2d is in lat,lon coords
  !>                                 False: vertex_coords_2d is in x,y coords
  !> @param[in]  nverts_per_2d_cell  Number of vertices per cell of the 2D
  !>                                 layer.
  !> @param[in]  nedges_per_2d_cell  Number of edges per cell of the 2D layer.
  !> @param[in]  nverts_2d           No. of vertices in a 2d layer
  !> @param[in]  nverts_3d           No. of vertices in 3d mesh
  !> @param[in]  ncells_2d           No. of cells in a 2d layer
  !> @param[in]  ncells_3d           No. of cells in 3d mesh
  !> @param[in]  nlayers             No. of layers
  !> @param[in]  dz                  Array of vertical grid spacing
  !> @param[in]  reference_element   Shape of cells in the mesh.
  !>
  subroutine mesh_extruder( cell_next,          &
                            vert_on_cell,       &
                            vertex_coords,      &
                            nfaces,             &
                            nverts,             &
                            cell_next_2d,       &
                            vert_on_cell_2d,    &
                            vertex_coords_2d,   &
                            is_spherical,       &
                            nverts_per_2d_cell, &
                            nedges_per_2d_cell, &
                            nverts_2d,          &
                            nverts_3d,          &
                            ncells_2d,          &
                            ncells_3d,          &
                            nlayers,            &
                            dz,                 &
                            reference_element )

    use coord_transform_mod,   only : llr2xyz
    use reference_element_mod, only : reference_element_type, &
                                      W, S, E, N, B, T, &
                                      SWB, SEB, NEB, NWB, SWT, SET, NET, NWT

    implicit none

    integer(i_def), intent(in)  :: nfaces
    integer(i_def), intent(in)  :: nverts
    integer(i_def), intent(in)  :: nverts_per_2d_cell
    integer(i_def), intent(in)  :: nedges_per_2d_cell
    integer(i_def), intent(in)  :: nverts_2d
    integer(i_def), intent(in)  :: nverts_3d
    integer(i_def), intent(in)  :: ncells_2d
    integer(i_def), intent(in)  :: ncells_3d
    integer(i_def), intent(in)  :: nlayers
    integer(i_def), intent(in)  :: cell_next_2d(nedges_per_2d_cell, ncells_2d)
    integer(i_def), intent(in)  :: vert_on_cell_2d(nverts_per_2d_cell, &
                                                   ncells_2d)
    real(r_def),    intent(in)  :: vertex_coords_2d( 3, nverts_2d )
    logical(l_def), intent(in)  :: is_spherical
    real(r_def),    intent(in)  :: dz( nlayers )
    integer(i_def), intent(out) :: cell_next( nfaces, ncells_3d )
    integer(i_def), intent(out) :: vert_on_cell( nverts, ncells_3d )
    real(r_def),    intent(out) :: vertex_coords( 3, nverts_3d )
    class(reference_element_type), &
                    intent(in)  :: reference_element

    ! Loop indices
    integer(i_def) :: i, j, k, id, jd, iedge, ivert, base_id

    integer(i_def) :: nfaces_h

    integer(i_def) :: nverts_per_3d_cell
    integer(i_def) :: nedges_per_3d_cell
    integer(i_def) :: nfaces_per_3d_cell

    ! lat/long coordinates
    real(r_def) :: long, lat, r

    ! Reference element stats
    nfaces_h = reference_element%get_number_horizontal_faces()

    nverts_per_3d_cell = nverts
    nedges_per_3d_cell = reference_element%get_number_edges()
    nfaces_per_3d_cell = nfaces

    ! Apply default cell_next values
    cell_next(:,:) = 0

    do i=1, ncells_2d
      do ivert=1, nverts_per_2d_cell
        vert_on_cell(ivert,i) = vert_on_cell_2d(ivert,i)
      end do

      do iedge=1, nedges_per_2d_cell
        cell_next(iedge,i) = cell_next_2d(iedge,i)
      end do
    end do

    ! Add connectivity for up/down
    ! index nfaces_h+1 is the bottom of the 3d cell
    !                  (set to zero as it's the surface)
    ! index nfaces_h+2 is the top of the 3d cell
    do j=1,ncells_2d
      cell_next(nfaces_h + 2, j) = j + ncells_2d
    end do

    ! Perform vertical extrusion for connectivity
    do k=1, nlayers-1
      do i=1, ncells_2d
        id = i  + k*ncells_2d
        jd = id - ncells_2d

        do j=1, nfaces_h ! Only over vertical faces
          if (cell_next(j,jd) /= 0) &
                    cell_next(j,id) = cell_next(j,jd) + &
                                              ncells_2d
        end do

        cell_next(nfaces_h + 1, id) = id - ncells_2d
        cell_next(nfaces_h + 2, id) = id + ncells_2d

        if (k==nlayers-1) cell_next(nfaces_h + 2, id) = 0

      end do
    end do

    ! The assumption is that the global mesh coords are provided in
    ! [longitude, latitude, radius] (long/lat in rads).

    ! Perform vertical extrusion for vertices
    do j=1, nverts_2d
     ! k = 0
     vertex_coords(1,j) = vertex_coords_2d(1,j)
     vertex_coords(2,j) = vertex_coords_2d(2,j)
     vertex_coords(3,j) = vertex_coords_2d(3,j)
     ! Rest of k, up to nlayers
      do k=1, nlayers
        vertex_coords(1,j+k*nverts_2d) = vertex_coords_2d(1,j)
        vertex_coords(2,j+k*nverts_2d) = vertex_coords_2d(2,j)
        vertex_coords(3,j+k*nverts_2d) = vertex_coords_2d(3,j) + sum(dz(1:k))
      end do
    end do

    if ( is_spherical ) then

      ! Convert (long,lat,r) -> (x,y,z)
      ! long,lat in radians
      do j=1, nverts_2d
        do k=0, nlayers
          long = vertex_coords(1,j+k*nverts_2d)
          lat  = vertex_coords(2,j+k*nverts_2d)
          r    = vertex_coords(3,j+k*nverts_2d)
          call llr2xyz(long,lat,r,vertex_coords(1,j+k*nverts_2d), &
                                  vertex_coords(2,j+k*nverts_2d), &
                                  vertex_coords(3,j+k*nverts_2d))
        end do
      end do
    end if

    ! Assign vertices to cells
    ! Loop over lowest layer of cells first, to set the cell ids above
    ! the lowest layer
    do i=1, ncells_2d
      do ivert=1, nverts_per_2d_cell
        vert_on_cell(nverts_per_2d_cell + ivert, i) = &
          vert_on_cell(ivert,i) + nverts_2d
      end do
    end do

    ! Do vertical extrusion
    do base_id=1, ncells_2d
      do k=1, nlayers-1

        i = base_id + k*ncells_2d
        do ivert=1, nverts_per_3d_cell
          vert_on_cell(ivert,i) = vert_on_cell(ivert,base_id) &
                                       + k*nverts_2d
        end do

      end do
    end do

    ! Diagnostic information from now on.
    if (log_level() > LOG_LEVEL_DEBUG) return

    call log_event('grid connectivity', LOG_LEVEL_DEBUG)
    do i=1, ncells_3d
      write(log_scratch_space,'(7i6)') i, &
        cell_next(S,i), cell_next(E,i), &
        cell_next(N,i), cell_next(W,i), &
        cell_next(B,i), cell_next(T,i)
      call log_event(log_scratch_space, LOG_LEVEL_DEBUG)
    end do

    call log_event('verts on cells', LOG_LEVEL_DEBUG)
    do i=1, ncells_3d
      write(log_scratch_space, '(9i6)') i, &
        vert_on_cell(SWB,i), vert_on_cell(SEB,i), &
        vert_on_cell(NEB,i), vert_on_cell(NWB,i), &
        vert_on_cell(SWT,i), vert_on_cell(SET,i), &
        vert_on_cell(NET,i), vert_on_cell(NWT,i)
      call log_event(log_scratch_space, LOG_LEVEL_DEBUG)
    end do

    call log_event('vert coords', LOG_LEVEL_DEBUG)
    do i=1, nverts_3d
      write(log_scratch_space, '(i6,3ES20.10E3)') i, &
        vertex_coords(1,i), &
        vertex_coords(2,i), &
        vertex_coords(3,i)
      call log_event(log_scratch_space, LOG_LEVEL_DEBUG)
    end do

    return
  end subroutine mesh_extruder

  !===========================================================================
  !> @brief Helper function to compute mesh connectivity.
  !>
  !> @param[out] face_on_cell    All face ids on any cell
  !> @param[out] edge_on_cell    All edge ids on any cell
  !> @param[in]  ncells_2d       Number of cells in 2D mesh.
  !> @param[in]  ncells_3d       Number of cells in 3D mesh.
  !> @param[in]  nfaces_per_cell Number of faces on 3d-cell
  !> @param[in]  nedges_per_cell Number of edges on 3d-cell
  !> @param[in]  cell_next       Cell ids of all cells adjacent to any cell
  !> @param[in]  vert_on_cell    Vertex ids on any cell
  !> @param[in]  reference_element Properties of an element in the mesh.
  !>
  subroutine mesh_connectivity( face_on_cell,    &
                                edge_on_cell,    &
                                ncells_2d,       &
                                ncells_3d,       &
                                nfaces_per_cell, &
                                nedges_per_cell, &
                                cell_next,       &
                                vert_on_cell,    &
                                reference_element )

    use reference_element_mod, only: reference_element_type,         &
                                     W,   S,  E,  N,  B,  T,         &
                                     EB, ET, WB, WT, NB, NT, SB, ST, &
                                     SW, SE, NW, NE,                 &
                                     SWB, SEB, NEB, NWB, SWT, SET, NET, NWT

    implicit none

    integer(i_def), intent(in)  :: ncells_2d
    integer(i_def), intent(in)  :: ncells_3d
    integer(i_def), intent(in)  :: nfaces_per_cell
    integer(i_def), intent(in)  :: nedges_per_cell
    integer(i_def), intent(in)  :: cell_next(:, :)
    integer(i_def), intent(in)  :: vert_on_cell(:, :)
    integer(i_def), intent(out) :: face_on_cell( nfaces_per_cell, ncells_2d )
    integer(i_def), intent(out) :: edge_on_cell( nedges_per_cell, ncells_2d )
    class(reference_element_type), &
                    intent(in)  :: reference_element

    integer (i_def) :: cell          ! cell loop index
    integer (i_def) :: face          ! face loop index
    integer (i_def) :: edge          ! edge loop index
    integer (i_def) :: vert          ! vert loop index
    integer (i_def) :: face_id       ! unique face index
    integer (i_def) :: edge_id       ! unique edge index
    integer (i_def) :: edge_upper    ! index of edge on top face
    integer (i_def) :: cell_nbr      ! cell index for a neightbour cell
    integer (i_def) :: face_nbr      ! face index for a face on a neighbour cell
    integer (i_def) :: edge_nbr      ! edge index for a edge on a neighbour cell
    integer (i_def) :: vert_nbr      ! vert index for a edge on a neighbour cell
    integer (i_def) :: cell_nbr_nbr  ! cell index for a neighbour cell of a neighbour cell
    integer (i_def) :: vert_nbr_nbr  ! vert index for a neighbour cell of a neighbour cell

    integer(i_def) :: nfaces_h
    integer(i_def) :: nedges_h
    integer(i_def) :: nverts_h

    nfaces_h = reference_element%get_number_horizontal_faces()
    nedges_h = reference_element%get_number_2d_edges()
    nverts_h = reference_element%get_number_2d_vertices()

    face_on_cell(:,:) = 0
    edge_on_cell(:,:) = 0

    !==================================================
    ! Compute index of faces on the cell
    !==================================================
    face_id = 1
    do cell=1, ncells_2d

      ! First do faces on E,S,W,N sides of cell
      do face=1, nfaces_h

        if ( face_on_cell(face,cell) == 0 ) then
          ! Assign face_id to this face as it is not already assigned

          face_on_cell(face,cell) = face_id

          ! Find matching face on the neighbouring cell.
          ! Note: The orientations of cells may not be the same,
          !       e.g. Cubedsphere, for a so search all faces of
          !       cell_next on the neighbouring cell
          cell_nbr = cell_next(face,cell)
          if (cell_nbr /= 0) then
            do face_nbr=1, nfaces_h
              if ( cell_next(face_nbr,cell_nbr) == cell ) then
                ! Found matching face, assign in and increment count
                face_on_cell(face_nbr,cell_nbr) = face_id
              end if
            end do
          end if

          face_id = face_id + 1

        end if ! Test for assigned face

      end do  ! n_faces_h

      ! Now do Faces on B and T of cell
      do face=nfaces_h+1, nfaces_per_cell
        face_on_cell(face, cell) = face_id
        face_id = face_id + 1
      end do

    end do ! ncells_2d

    ! Compute the index of edges on the cell
    edge_id = 1
    do cell=1, ncells_2d
      ! horizontal edges ( edges on Bottom and Top faces)
      ! This uses the fact that edges of the bottom face correspond to the
      ! vertical faces, i.e edge i is the bottom edge of face i and hence
      ! we can use cell_next array (which is addressed through faces) for
      ! edge indexes
      do edge=1, nedges_h
        if ( edge_on_cell(edge,cell) == 0 ) then

          ! Index of edge on bottom face
          edge_on_cell(edge,cell) = edge_id

          ! Corresponding index of edge on top face
          edge_upper = edge + nedges_h + nverts_h
          edge_on_cell(edge_upper,cell) = edge_id + 1

          ! Find matching edge in neighbouring cell
          cell_nbr = cell_next(edge,cell)
          if (cell_nbr /= 0) then
            do edge_nbr = 1, nedges_h
              if ( cell_next(edge_nbr,cell_nbr) == cell ) then
                ! Found cell which shares edge
                edge_on_cell(edge_nbr,cell_nbr) = edge_id
                edge_upper = edge_nbr + nedges_h + nverts_h
                edge_on_cell(edge_upper,cell_nbr) = edge_id + 1
              end if
            end do
          end if

          edge_id = edge_id + 2

        end if
      end do

      ! Vertical edges (edges not on Bottom and Top faces)
      ! This uses the fact that vertical edges correspond to vertices of
      ! the bottom face i.e edge i has the same index as vertex i and hence
      ! we can use vert_on_cell array (which is addressed through verts) for
      ! edge indexes
      do edge = nedges_h+1,nedges_h+nverts_h
        if ( edge_on_cell(edge,cell) == 0 ) then
          edge_on_cell(edge,cell) = edge_id

          ! Find matching edge on two neighbouring cells
          ! This edge is an extrusion of the corresponding vertex
          vert = vert_on_cell(edge-nedges_h,cell)
          do face = 1,nfaces_h
            cell_nbr = cell_next(face,cell)
            if (cell_nbr /= 0) then
              do vert_nbr = 1,nverts_h
                if ( vert_on_cell(vert_nbr,cell_nbr) == vert ) then
                  ! Found matching vert in neighbour cell
                  edge_on_cell(vert_nbr+nedges_h,cell_nbr) = edge_id
                  do face_nbr = 1,nfaces_h
                    ! Now find matching vert in neighbour of neighbour
                    cell_nbr_nbr = cell_next(face_nbr,cell_nbr)
                    if (cell_nbr_nbr /= 0) then
                      do vert_nbr_nbr = 1,nverts_h
                        if ( vert_on_cell(vert_nbr_nbr,cell_nbr_nbr) == &
                             vert ) then
                          edge_on_cell(vert_nbr_nbr+nedges_h, &
                                            cell_nbr_nbr) = edge_id
                        end if
                      end do
                    end if ! cell_nbr_nbr
                  end do
                end if
              end do
            end if
          end do
          edge_id = edge_id + 1
        end if
      end do
    end do

    ! Diagnostic information from now on.
    if (log_level() > LOG_LEVEL_DEBUG) return

    call log_event( 'faces on cells', LOG_LEVEL_DEBUG )
    do cell = 1, ncells_2d
      write( log_scratch_space, '(7i6)' ) cell, &
             face_on_cell(S,cell), face_on_cell(E,cell), &
             face_on_cell(N,cell), face_on_cell(W,cell), &
             face_on_cell(B,cell), face_on_cell(T,cell)
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    end do

    call log_event( 'edges on cells', LOG_LEVEL_DEBUG )
    do cell = 1, ncells_2d
      write( log_scratch_space, '(13i6)' ) cell, &
             edge_on_cell(SB,cell), edge_on_cell(EB,cell), &
             edge_on_cell(NB,cell), edge_on_cell(WB,cell), &
             edge_on_cell(SW,cell), edge_on_cell(SE,cell), &
             edge_on_cell(NE,cell), edge_on_cell(NW,cell), &
             edge_on_cell(ST,cell), edge_on_cell(ET,cell), &
             edge_on_cell(NT,cell), edge_on_cell(WT,cell)
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    end do

    return
  end subroutine mesh_connectivity

  !===========================================================================
  !> @brief Helper function to compute the domain limits (x,y,z) for Cartesian
  !>        domains and (lambda,phi,r) for spherical domains.
  !>
  !> @param[out] domain_size   Domain limits in 3d
  !> @param[in]  domain_top    Top of 3D domain in meters.
  !> @param[in]  vertex_coords Vertex Coordinates
  !> @param[in]  nverts        No. of vertices in 3d mesh
  !>
  subroutine set_domain_size( domain_size, domain_top, &
                              vertex_coords, nverts, &
                              is_spherical, scaled_radius )

    implicit none

    type(domain_size_type), intent(out) :: domain_size
    real(r_def),            intent(in)  :: domain_top
    integer(i_def),         intent(in)  :: nverts
    real(r_def),            intent(in)  :: vertex_coords(3,nverts)
    logical(l_def),         intent(in)  :: is_spherical
    real(r_def),            intent(in)  :: scaled_radius

    if ( is_spherical ) then
      domain_size%minimum%x   =  0.0_r_def
      domain_size%maximum%x   =  2.0_r_def*PI
      domain_size%minimum%y   = -0.5_r_def*PI
      domain_size%maximum%y   =  0.5_r_def*PI
      domain_size%minimum%z   =  0.0_r_def
      domain_size%maximum%z   =  domain_top
      domain_size%base_height =  scaled_radius
    else
      domain_size%minimum%x   =  planar_domain_min_x
      domain_size%maximum%x   =  planar_domain_max_x
      domain_size%minimum%y   =  planar_domain_min_y
      domain_size%maximum%y   =  planar_domain_max_y
      domain_size%minimum%z   =  minval( vertex_coords(3,:))
      domain_size%maximum%z   =  maxval( vertex_coords(3,:))
      domain_size%base_height =  0.0_r_def
    end if

    !> @todo Need to do a global reduction of maxs and mins when the
    !> code is parallel

    return
  end subroutine set_domain_size

  !============================================================================
  !> @brief Helper function that calculates and stores depth of layers in
  !> 3d_cell column given the suface height and the domain top (called from
  !> the mesh constructor).
  !> @param[in,out] dz                Depth of 3d-cell layer
  !> @param[in]    eta                Non-dimensional vertical coordinate
  !> @param[in]    nlayers            Number of layers
  !> @param[in]    surface_height     Surface height above flat surface
  !> @param[in]    domain_top         Top of atmosphere above flat surface
  subroutine set_dz( dz,             &
                     eta,            &
                     nlayers,        &
                     surface_height, &
                     domain_top )

    implicit none

    integer(i_def), intent(in)    :: nlayers
    real(r_def),    intent(in)    :: surface_height
    real(r_def),    intent(in)    :: domain_top
    real(r_def),    intent(inout) :: dz(nlayers)
    real(r_def),    intent(in)    :: eta(0:nlayers)

    real(r_def)    :: domain_depth

    ! NOTE: dz and domain top will depend on
    !       the orography, vertical resolution file, top of model
    !       and how vertical and horizontal smoothing is applied
    !       after the application of orography.

    ! Calculate domain depth
    domain_depth = domain_top - surface_height
    ! Calculate dz
    dz = ( eta(1:nlayers) - eta(0:nlayers-1) )*domain_depth

    return
  end subroutine set_dz

end module mesh_constructor_helper_functions_mod
