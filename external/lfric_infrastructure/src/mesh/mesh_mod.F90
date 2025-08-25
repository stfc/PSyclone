!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief Local 3D mesh object.
!>
!> This module provides details for a mesh_type which is generated using a
!> local_mesh object along with some inputs that describe the vertical
!> structure.
!>
!> It also contains a static mesh object for unit testing. This is returned
!> if a mesh_object is instantiated with an integer argument and a local mesh.
!>
module mesh_mod

  use constants_mod,         only : i_def, r_def, l_def, str_def, &
                                    pi, imdi
  use domain_mod,            only : domain_type
  use extrusion_mod,         only : extrusion_type
  use lfric_mpi_mod,         only : global_mpi
  use linked_list_mod,       only : linked_list_type, &
                                    linked_list_item_type
  use linked_list_data_mod,  only : linked_list_data_type
  use local_mesh_map_mod,    only : local_mesh_map_type
  use local_mesh_mod,        only : local_mesh_type
  use log_mod,               only : log_event, log_scratch_space, &
                                    LOG_LEVEL_ERROR, LOG_LEVEL_TRACE, &
                                    LOG_LEVEL_INFO, LOG_LEVEL_DEBUG
  use mesh_colouring_mod,    only : set_colours
  use mesh_tiling_mod,       only : set_tiling
  use mesh_constructor_helper_functions_mod,           &
                             only : mesh_extruder,     &
                                    mesh_connectivity, &
                                    set_dz
  use mesh_map_mod,          only : mesh_map_type
  use mesh_map_collection_mod, only : mesh_map_collection_type
  use partition_mod,         only : partition_type
  use reference_element_mod, only : reference_element_type, &
                                    reference_prism_type, &
                                    reference_cube_type

  implicit none

  private

  !============================================================================
  ! Declare type definitions in this module
  !============================================================================

  type, extends(linked_list_data_type), public :: mesh_type

    private

    !> Mesh name
    character(str_def) :: mesh_name

    !> Describes the shape of an element on this mesh.
    class(reference_element_type), allocatable :: reference_element

    !> The local_mesh object that describes the local mesh this mesh
    !> is built on
    type(local_mesh_type), pointer :: local_mesh

    !> The domain limits (x,y,z) for Cartesian domains
    !>                   (long, lat, radius) for spherical
    type(domain_type) :: domain

    !> Number of 3d-cell layers in mesh object
    integer(i_def) :: nlayers

    !> Top of atmosphere above surface
    real(r_def) :: domain_depth

    !> Base surface height
    !  (0.0 for planar meshes, scaled_radius for cubedsphere)
    real(r_def) :: domain_base_height

    !> Label for the type of extrusion the mesh has
    integer(i_def) :: extrusion_id

    !> Non-dimensional vertical coordinate eta[0,1], eta(0:nlayers)
    real(r_def), allocatable :: eta(:)

    !> Depth of 3d-cell layer [m], dz(nlayers)
    real(r_def), allocatable :: dz(:)

    !> Vertex Coordinates
    !> The x-, y- and z-coordinates of vertices on the mesh [m]
    real(r_def), allocatable :: vertex_coords(:,:)

    !> Cell-centre Coordinates
    !> The x-, y- and z-coordinates of the centres of cells in the mesh
    real(r_def), allocatable :: cell_centre_coords(:,:)

    !==========================================================================
    ! Mesh properties:
    !==========================================================================
    ! Local partition base level
    integer(i_def) :: nverts_2d            !< Number of verts in partition
    integer(i_def) :: nedges_2d            !< Number of edges in partition
    integer(i_def) :: ncells_2d            !< Number of cells in partition
    integer(i_def) :: ncells_2d_with_ghost !< Number of cells in partition
                                           !< @b including ghost cells

    integer(i_def) :: nverts_per_2d_cell   ! Number of vertices per face
    integer(i_def) :: nverts_per_edge      ! Number of vertices per edge
    integer(i_def) :: nedges_per_2d_cell   ! Number of edges per face

    ! Local partition 3d-mesh
    integer(i_def) :: nverts               !< Total number of verts in mesh
    integer(i_def) :: nedges               !< Total number of edges in mesh
    integer(i_def) :: nfaces               !< Total number of faces in mesh
    integer(i_def) :: ncells               !< Total number of cells in mesh
                                           !< @b excluding ghost cells
    integer(i_def) :: ncells_with_ghost    !< Total number of cells in mesh
                                           !< @b including ghost cells

    ! 3D-element properties
    integer(i_def) :: nverts_per_cell      !< Number of verts on 3d-cell
    integer(i_def) :: nedges_per_cell      !< Number of edges on 3d-cell
    integer(i_def) :: nfaces_per_cell      !< Number of faces on 3d-cell

    !==========================================================================
    ! Connectivities
    !==========================================================================
    ! All vertex, edge, face and cell id numbers for connectivitivies are
    ! LOCAL (lid) unless explicitly stated in the variable name,
    ! i.e. gid (GLOBAL id numbers)

    ! All connectivity arrays are in the form (ids of entities, cell local id)
    ! i.e. cell_next(5,6) would hold the local id of the cell adjacent to
    !      face-5 of 3d-cell with local id = 6. Face-5 of a 3d-cell is the
    !      bottom so would be the 3d-cell directly below.

    ! For 3d-mesh
    !> Cell ids of adjacent cells
    integer(i_def), allocatable :: cell_next    (:,:)

    !> Vertex ids on cell
    integer(i_def), allocatable :: vert_on_cell (:,:)

    !> Face ids on cell
    integer(i_def), allocatable :: face_on_cell (:,:)

    !> Edge ids on cell
    integer(i_def), allocatable :: edge_on_cell (:,:)

    !> The cell that "owns" the vertex entities around each cell
    integer(i_def), allocatable :: vert_cell_owner(:,:)

    !> The cell that "owns" the edge entities around each cell
    integer(i_def), allocatable :: edge_cell_owner(:,:)

    !> The rank of the partition that "owns" the
    !> vertex entities around each cell
    integer(i_def), allocatable :: vertex_ownership(:,:)

    !> The rank of the partition that "owns" the
    !> edge entities around each cell
    integer(i_def), allocatable :: edge_ownership(:,:)

    !> Local index of face in adjacent cells
    integer(i_def), allocatable :: face_id_in_adjacent_cell(:,:)

    !==========================================================================
    ! Colouring storage: these form the arguments to set_colours().
    !==========================================================================
    !> integer, the number of colours
    integer(i_def),              private :: ncolours
    !> integer 1-d array, how many cells belong to each colour
    integer(i_def), allocatable, private :: ncells_per_colour(:)
    !> integer 2-d array, which cells are in each colour.
    integer(i_def), allocatable, private :: cells_in_colour(:,:)
    !> integer 2-d array, how many of the first so many cells belong to each colour
    integer(i_def), allocatable, private :: ncells_per_colour_subset(:,:)
    integer(i_def),allocatable           :: last_inner_cell_per_colour(:,:)
    integer(i_def),allocatable           :: last_halo_cell_per_colour(:,:)
    integer(i_def),allocatable           :: last_edge_cell_per_colour(:)
    !==========================================================================
    ! Maps that this mesh connects to
    !
    type(mesh_map_collection_type), allocatable :: mesh_maps

    !==========================================================================
    ! Tiling storage
    !==========================================================================
    !> The number of colours used for tiling
    integer(i_def)              :: ntilecolours
    !> The number of tiles that belong to each colour
    integer(i_def), allocatable :: ntiles_per_colour(:)
    !> The number of cells that belong to each coloured tile
    integer(i_def), allocatable :: ncells_per_coloured_tile(:,:)
    !> Which cells belong to which coloured tile
    integer(i_def), allocatable :: cells_in_coloured_tile(:,:,:)
    !> How many tiles include cells up to a given halo or partition edge
    integer(i_def), allocatable :: last_inner_tile_per_colour(:,:)
    integer(i_def), allocatable :: last_edge_tile_per_colour(:)
    integer(i_def), allocatable :: last_halo_tile_per_colour(:,:)
    !> How many cells in a tile are inside a given halo or partition edge
    integer(i_def), allocatable :: last_inner_cell_per_coloured_tile(:,:,:)
    integer(i_def), allocatable :: last_halo_cell_per_coloured_tile(:,:,:)
    integer(i_def), allocatable :: last_edge_cell_per_coloured_tile(:,:)

  contains

    procedure, public :: get_reference_element
    procedure, public :: get_mesh_name
    procedure, public :: get_local_mesh
    procedure, public :: get_nlayers
    procedure, public :: get_ncells_2d
    procedure, public :: get_ncells_2d_with_ghost
    procedure, public :: get_nedges_2d
    procedure, public :: get_nverts_2d
    procedure, public :: get_ncells
    procedure, public :: get_nverts
    procedure, public :: get_nedges
    procedure, public :: get_nfaces
    procedure, public :: get_vert_coords
    procedure, public :: get_cell_centre_coords
    procedure, public :: get_cell_coords
    procedure, public :: get_column_coords
    procedure, public :: get_nverts_per_cell
    procedure, public :: get_nverts_per_cell_2d
    procedure, public :: get_nverts_per_edge
    procedure, public :: get_nedges_per_cell
    procedure, public :: get_nedges_per_cell_2d
    procedure, public :: get_nfaces_per_cell
    procedure, public :: get_cell_next
    procedure, public :: get_face_on_cell
    procedure, public :: get_edge_on_cell
    procedure, public :: get_vert_on_cell
    procedure, public :: get_domain
    procedure, public :: get_domain_top
    procedure, public :: get_extrusion_id
    procedure, public :: get_dz
    procedure, public :: get_eta
    procedure, public :: get_vertex_cell_owner
    procedure, public :: get_edge_cell_owner
    procedure, public :: is_vertex_owned
    procedure, public :: is_edge_owned
    procedure, public :: is_cell_owned

    procedure, public :: is_geometry_spherical
    procedure, public :: is_geometry_planar
    procedure, public :: is_topology_non_periodic
    procedure, public :: is_topology_channel
    procedure, public :: is_topology_periodic
    procedure, public :: is_coord_sys_xyz
    procedure, public :: is_coord_sys_ll

    procedure, public :: get_num_edges_owned_2d
    procedure, public :: get_num_verts_owned_2d
    procedure, public :: get_inner_depth
    procedure, public :: get_num_cells_inner

    procedure, public :: get_last_inner_cell
    procedure, public :: get_last_inner_cell_per_colour
    procedure, public :: get_last_inner_cell_all_colours
    procedure, public :: get_last_inner_tile_per_colour
    procedure, public :: get_last_inner_tile_all_colours
    procedure, public :: get_last_inner_cell_per_colour_and_tile
    procedure, public :: get_last_inner_cell_all_colours_all_tiles

    procedure, public :: get_num_cells_edge

    procedure, public :: get_last_edge_cell
    procedure, public :: get_last_edge_cell_per_colour
    procedure, public :: get_last_edge_cell_all_colours
    procedure, public :: get_last_edge_tile_per_colour
    procedure, public :: get_last_edge_tile_all_colours
    procedure, public :: get_last_edge_cell_per_colour_and_tile
    procedure, public :: get_last_edge_cell_all_colours_all_tiles

    procedure, public :: get_halo_depth
    procedure, public :: get_num_cells_halo

    procedure, public :: get_last_halo_cell_any
    procedure, public :: get_last_halo_cell_deepest
    generic           :: get_last_halo_cell => &
                           get_last_halo_cell_any, &
                           get_last_halo_cell_deepest

    procedure, public :: get_last_halo_cell_per_colour_any
    procedure, public :: get_last_halo_cell_per_colour_deepest
    generic           :: get_last_halo_cell_per_colour => &
                           get_last_halo_cell_per_colour_any, &
                           get_last_halo_cell_per_colour_deepest

    procedure, public :: get_last_halo_cell_all_colours
    procedure, public :: get_last_halo_cell_all_colours_deepest

    procedure, public :: get_last_halo_tile_per_colour_any
    procedure, public :: get_last_halo_tile_per_colour_deepest
    generic           :: get_last_halo_tile_per_colour => &
                           get_last_halo_tile_per_colour_any, &
                           get_last_halo_tile_per_colour_deepest

    procedure, public :: get_last_halo_tile_all_colours
    procedure, public :: get_last_halo_tile_all_colours_deepest

    procedure, public :: get_last_halo_cell_per_colour_and_tile_any
    procedure, public :: get_last_halo_cell_per_colour_and_tile_deepest
    generic           :: get_last_halo_cell_per_colour_and_tile => &
                           get_last_halo_cell_per_colour_and_tile_any, &
                           get_last_halo_cell_per_colour_and_tile_deepest

    procedure, public :: get_last_halo_cell_all_colours_all_tiles
    procedure, public :: get_last_halo_cell_all_colours_all_tiles_deepest

    procedure, public :: get_num_cells_ghost
    procedure, public :: get_gid_from_lid
    procedure, public :: get_mesh_map
    procedure, public :: query_mesh_map
    procedure, public :: add_mesh_map
    procedure, public :: get_adjacent_face

    ! Get information about colouring of mesh
    procedure, public :: get_ncolours
    procedure, public :: get_colours
    procedure, public :: get_colour_map
    procedure, public :: is_coloured

    ! Get information about mesh tiling
    procedure, public :: get_ntilecolours
    procedure, public :: get_tiling
    procedure, public :: get_coloured_tiling_map

    procedure, public :: debug
    procedure, public :: clear

    ! Destructor
    final :: mesh_destructor

  end type mesh_type

  interface mesh_type
    module procedure mesh_constructor
    module procedure mesh_constructor_unit_test_data
  end interface

  ! -------------------------------------------------------------------------
  ! Module parameters
  ! -------------------------------------------------------------------------

  !> Counter variable to keep track of the next mesh id number to uniquely
  !! identify each different mesh
  integer(i_def), save :: mesh_id_counter = 0

  !============================================================================
  ! Options for horizontal pFUnit test meshes
  !============================================================================
  !
  !> @}
  !> @name Horizontal Grid Types for pFunit tests
  integer(i_def), parameter, public :: PLANE                  = 1
  integer(i_def), parameter, public :: PLANE_BI_PERIODIC      = 2
  integer(i_def), parameter, public :: PLANE_TWOD             = 3
  integer(i_def), parameter, public :: PLANE_TWOD_BI_PERIODIC = 4
  !> @}

contains

  !============================================================================
  !> @brief Constructor for the mesh object
  !> @param [in] local_mesh    A pointer to a local mesh object holding 2d
  !>                           information about the partitioned mesh
  !> @param [in] extrusion     Mechnism by which extrusion is to be achieved.
  !> @param [in, optional]
  !>             mesh_name     Mesh tag name to use for this mesh. If omitted,
  !>                           the mesh name from the local mesh will be used.
  !> @param [in, optional]
  !>             tile_size     Sets tile size to (mxn) cells, activates tiling
  !> @param [in, optional]
  !>      inner_halo_tiles     Tile inner halos separately for overlapping
  !                            computation and communication
  !> @return                   3D-Mesh object based on the list of partitioned
  !>                           cells on the given local mesh
  !============================================================================
  function mesh_constructor ( local_mesh,         &
                              extrusion,          &
                              mesh_name,          &
                              tile_size,          &
                              inner_halo_tiles )  &
                              result( self )

    implicit none

    type(local_mesh_type),  intent(in), pointer  :: local_mesh
    class(extrusion_type),  intent(in)           :: extrusion
    character(str_def),     intent(in), optional :: mesh_name
    integer(i_def),         intent(in), optional :: tile_size(2)
    logical(l_def),         intent(in), optional :: inner_halo_tiles

    type(mesh_type) :: self

    ! Loop counters
    integer(i_def) :: i, j



    ! Vertices connected to local 2d cell.
    integer(i_def), allocatable :: vert_on_cell_2d (:,:)

    ! Edges connected to local 2d cell.
    integer(i_def), allocatable :: edge_on_cell_2d (:,:)

    ! Local 2d cell connectivity.
    integer(i_def), allocatable :: cell_next_2d (:,:)

    ! Surface Coordinates in [long, lat, radius] (Units: Radians/metres)
    real(r_def), allocatable :: vertex_coords_2d(:,:)
    real(r_def), allocatable :: cell_centre_coords_2d(:,:)

    ! ll_coords = True: vertex_coords_2d/cell_centre_coords_2d are in lat,lon coords,
    !             False: vertex_coords_2d/cell_centre_coords_2d are in x,y coords
    logical(l_def) :: ll_coords

    character(str_def):: name

    ! Mesh tiling parameters - these assume defaults if not set
    integer(i_def) :: tile_size_xy(2)
    logical(l_def) :: sep_inner_halo_tiles

    ! Get unique id for the mesh
    mesh_id_counter = mesh_id_counter+1
    call self%set_id( mesh_id_counter )

    if (present(mesh_name)) then
      name = mesh_name
    else
      name = local_mesh%get_mesh_name()
    end if

    self%mesh_name = name

    ! Instantiate appropriate reference element
    select case (local_mesh%get_nverts_per_cell())
      case (3)
        allocate( self%reference_element, source=reference_prism_type() )
      case (4)
        allocate( self%reference_element, source=reference_cube_type() )
      case default
        write( log_scratch_space, &
              '("Base mesh with ", I0, " vertices per cell not supported.")' &
             ) local_mesh%get_nverts_per_cell()
        call log_event( log_scratch_space, log_level_error )
    end select

    self%nverts_per_2d_cell = local_mesh%get_nverts_per_cell()
    self%nverts_per_edge = local_mesh%get_nverts_per_edge()
    self%nedges_per_2d_cell = local_mesh%get_nedges_per_cell()

    self%local_mesh           => local_mesh

    self%ncells_2d            = local_mesh%get_num_cells_in_layer()
    self%ncells_2d_with_ghost = self%ncells_2d &
                                 + local_mesh%get_num_cells_ghost()
    self%nlayers              = extrusion%get_number_of_layers()
    self%ncells               = self%ncells_2d * self%nlayers
    self%ncells_with_ghost    = self%ncells_2d_with_ghost * self%nlayers
    self%domain_depth         = extrusion%get_atmosphere_top()
    self%domain_base_height   = extrusion%get_atmosphere_bottom()
    self%extrusion_id         = extrusion%get_id()
    self%ncolours             = -1     ! Initialise ncolours to error status

    allocate( self%eta ( 0:self%nlayers ) )
    allocate( self%dz  ( self%nlayers   ) )

    ! Calculate non-dimensional vertical coordinate eta[0,1]
    call extrusion%extrude( self%eta )

    ! Calculate layer depth dz for flat planet surface
    call set_dz( self%dz,      &
                 self%eta,     &
                 self%nlayers, &
                 0.0_r_def,    &
                 self%domain_depth )

    ! Calculate next-to cells and vertices on cells
    allocate( self%cell_next(self%reference_element%get_number_faces(), &
                             self%ncells_with_ghost) )
    allocate( self%vert_on_cell(self%reference_element%get_number_vertices(), &
              self%ncells_with_ghost) )

    self%nverts_per_cell = 2*self%nedges_per_2d_cell
    self%nedges_per_cell = 2*self%nedges_per_2d_cell + self%nverts_per_2d_cell
    self%nfaces_per_cell = self%nedges_per_2d_cell + 2

    !--------------------------------------------------------------------------
    ! Now get a list of all unique entities in partition (cells/vertices/edges)
    !--------------------------------------------------------------------------

    allocate( cell_next_2d (self%nedges_per_2d_cell, self%ncells_2d_with_ghost) )
    do i=1, self%ncells_2d_with_ghost
      call local_mesh%get_cell_next(i,cell_next_2d(:,i))
    end do

    ! Mesh colouring/tiling is written such that it is expecting 0_i_def for connectivity
    ! outside the local partition. So need to replace cell_next_2d void values with 0_i_def
    where ( cell_next_2d == local_mesh%get_void_cell() ) cell_next_2d = 0_i_def

    allocate( vert_on_cell_2d ( self%nverts_per_2d_cell, &
                                self%ncells_2d_with_ghost ) )
    do i=1, self%ncells_2d_with_ghost
      do j=1, self%nverts_per_2d_cell
        vert_on_cell_2d(j,i) = local_mesh%get_vert_on_cell(j,i)
      end do
    end do
    self%nverts_2d = local_mesh%get_n_unique_vertices()

    allocate( &
        edge_on_cell_2d (self%nedges_per_2d_cell, self%ncells_2d_with_ghost) )
    do i=1, self%ncells_2d_with_ghost
      do j=1, self%nedges_per_2d_cell
        edge_on_cell_2d(j,i) = local_mesh%get_edge_on_cell(j,i)
      end do
    end do
    self%nedges_2d = local_mesh%get_n_unique_edges()

    !--------------------------------------------------------------------------
    ! Get partition vertices lat-lon-z coords, Note z=surface height
    !--------------------------------------------------------------------------
    allocate(vertex_coords_2d(3,self%nverts_2d))
    vertex_coords_2d = 0.0_r_def
    do i=1, self%nverts_2d
      ! Get coords of vertices
      call local_mesh%get_vert_coords(i,vertex_coords_2d(1:2,i))
    end do

    !--------------------------------------------------------------------------
    ! Get partition cell-centre lat-lon-z coords, Note z=surface height
    !--------------------------------------------------------------------------
    allocate(cell_centre_coords_2d(3,self%ncells_2d_with_ghost))
    cell_centre_coords_2d = 0.0_r_def
    do i=1, self%ncells_2d_with_ghost
      ! Get coords of cell centres
      call local_mesh%get_cell_coords(i,cell_centre_coords_2d(1:2,i))
    end do

    ll_coords = .false.
    if ( local_mesh%is_coord_sys_ll() ) ll_coords = .true.

    ! Set base surface height
    vertex_coords_2d(3,:) = self%domain_base_height
    cell_centre_coords_2d(3,:) = self%domain_base_height

    self%nverts = self%nverts_2d * (self%nlayers+1)
    self%nedges = self%nedges_2d * (self%nlayers+1) &
                  + self%nverts_2d*self%nlayers
    self%nfaces = self%ncells_2d_with_ghost * (self%nlayers+1) &
                  + self%nedges_2d * self%nlayers


    allocate ( self%vertex_coords( 3, self%nverts ) )
    allocate ( self%cell_centre_coords( 3, self%ncells_with_ghost ) )

    call mesh_extruder( self%cell_next,                               &
                        self%vert_on_cell,                            &
                        self%vertex_coords,                           &
                        self%cell_centre_coords,                      &
                        self%reference_element%get_number_faces(),    &
                        self%reference_element%get_number_vertices(), &
                        cell_next_2d,                                 &
                        vert_on_cell_2d,                              &
                        vertex_coords_2d,                             &
                        cell_centre_coords_2d,                        &
                        ll_coords,                                    &
                        self%nverts_per_2d_cell,                      &
                        self%nedges_per_2d_cell,                      &
                        self%nverts_2d,                               &
                        self%nverts,                                  &
                        self%ncells_2d_with_ghost,                    &
                        self%ncells_with_ghost,                       &
                        self%nlayers,                                 &
                        self%dz,                                      &
                        self%reference_element )

    allocate( self%face_on_cell( self%nfaces_per_cell, &
                                 self%ncells_2d_with_ghost ) )
    allocate( self%edge_on_cell( self%nedges_per_cell, &
                                 self%ncells_2d_with_ghost ) )

    call mesh_connectivity( self%face_on_cell,         &
                            self%edge_on_cell,         &
                            self%ncells_2d_with_ghost, &
                            self%nfaces_per_cell,      &
                            self%nedges_per_cell,      &
                            self%cell_next,            &
                            self%vert_on_cell,         &
                            self%reference_element )

    self%domain = domain_type( local_mesh%get_global_domain_extents(), &
                               self%domain_base_height,                &
                               self%domain_depth, ll_coords )

    deallocate (vert_on_cell_2d)
    deallocate (edge_on_cell_2d)
    deallocate (cell_next_2d)
    deallocate (vertex_coords_2d)
    deallocate (cell_centre_coords_2d)

    ! Assign ownership of cell vertices and cell edges
    allocate( &
      self%vertex_ownership (self%nverts_per_2d_cell, self%ncells_2d_with_ghost) )
    allocate( &
      self%edge_ownership   (self%nedges_per_2d_cell, self%ncells_2d_with_ghost) )
    allocate( &
      self%vert_cell_owner  (self%nverts_per_2d_cell, self%ncells_2d_with_ghost) )
    allocate( &
      self%edge_cell_owner  (self%nedges_per_2d_cell, self%ncells_2d_with_ghost) )

    do i=1, self%ncells_2d_with_ghost
      ! Vertex ownership
      do j=1, self%nverts_per_2d_cell
        self%vert_cell_owner(j,i) = &
            local_mesh%get_vert_cell_owner(local_mesh%get_vert_on_cell(j,i))
        if (self%vert_cell_owner(j,i) > 0) then
          self%vertex_ownership(j,i) = local_mesh%get_cell_owner( &
                                                     self%vert_cell_owner(j,i) )
        else
          self%vertex_ownership(j,i) = global_mpi%get_comm_size() + 1
        end if
      end do
      ! Edge ownership
      do j=1, self%nedges_per_2d_cell
        self%edge_cell_owner(j,i) = &
            local_mesh%get_edge_cell_owner(local_mesh%get_edge_on_cell(j,i))
        if (self%edge_cell_owner(j,i) > 0) then
          self%edge_ownership(j,i) = local_mesh%get_cell_owner( &
                                                     self%edge_cell_owner(j,i) )
        else
          self%edge_ownership(j,i) = global_mpi%get_comm_size() + 1
        end if
      end do
    end do

    if (.not. allocated(self%mesh_maps) ) &
        allocate ( self%mesh_maps, source = mesh_map_collection_type() )

    if ( .not. allocated(self%face_id_in_adjacent_cell) )           &
      allocate ( self%face_id_in_adjacent_cell( self%nedges_per_2d_cell, &
                                                self%ncells_2d_with_ghost) )

    call calc_face_id_in_adjacent_cell(                                                 &
                                  self%face_id_in_adjacent_cell,                        &
                                  self%reference_element%get_number_horizontal_faces(), &
                                  self%cell_next,                                       &
                                  self%reference_element%get_number_faces(),            &
                                  self%ncells_2d_with_ghost )

    call set_colours( self%get_ncells_2d(),                                 &
                      self%cell_next,                                       &
                      self%ncolours,                                        &
                      self%ncells_per_colour,                               &
                      self%cells_in_colour,                                 &
                      self%reference_element%get_number_horizontal_faces(), &
                      self%local_mesh )

    call init_last_cell_per_colour(self)

    ! Set tiling to 1x1 tile size (equivalent to colouring) by default
    if ( present(tile_size) ) then
      tile_size_xy = tile_size
    else
      tile_size_xy = 1
    end if

    ! Tile partition as a whole by default (outer halos are always separated)
    if ( present(inner_halo_tiles) ) then
      sep_inner_halo_tiles = inner_halo_tiles
    else
      sep_inner_halo_tiles = .false.
    end if

    call set_tiling( self%get_ncells_2d(),                                  &
                     self%cell_next,                                        &
                     self%local_mesh,                                       &
                     self%ncolours,                                         &
                     self%ncells_per_colour,                                &
                     self%cells_in_colour,                                  &
                     tile_size_xy,                                          &
                     sep_inner_halo_tiles,                                  &
                     self%ntilecolours,                                     &
                     self%ntiles_per_colour,                                &
                     self%ncells_per_coloured_tile,                         &
                     self%cells_in_coloured_tile,                           &
                     self%last_inner_tile_per_colour,                       &
                     self%last_inner_cell_per_coloured_tile,                &
                     self%last_edge_tile_per_colour,                        &
                     self%last_edge_cell_per_coloured_tile,                 &
                     self%last_halo_tile_per_colour,                        &
                     self%last_halo_cell_per_coloured_tile )

  end function mesh_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the reference element for this mesh.
  !>
  function get_reference_element( self )

    implicit none

    class(mesh_type), intent(in), target :: self
    ! Return variable
    class(reference_element_type), pointer :: get_reference_element

    get_reference_element => self%reference_element

  end function get_reference_element

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets a pointer to the local_mesh object that was used to
  !>        create this mesh.
  !>
  !> @return Pointer to local_mesh object.
  !>
  function get_local_mesh(self) result(local_mesh)

    implicit none
    class(mesh_type),     target  :: self
    type(local_mesh_type), pointer :: local_mesh

    local_mesh => self%local_mesh

  end function get_local_mesh

  !============================================================================
  ! Mesh Type Methods
  !============================================================================
  !> @details This subrotuine returns 3-element array of vertex coords
  !>          in cartesian coords [x,y,z] with units in [m].
  !> @param[in]   vert_lid      The local id of the requested vertex
  !> @param[out]  vertex_coords A three-element array containing the
  !>                            cartesian coordinates of a single vertex
  !>                            in the mesh object
  !============================================================================
  subroutine get_vert_coords(self, vert_lid, vertex_coords)

    ! Returns 3-element array of vertex coords in
    ! cartesian coords [x,y,z] with units in [m].

    implicit none
    class(mesh_type), intent(in)  :: self
    integer(i_def),   intent(in)  :: vert_lid
    real(r_def),      intent(out) :: vertex_coords(:)

    vertex_coords(:) = self%vertex_coords(:,vert_lid)

  end subroutine get_vert_coords

  !> @details Returns 3-element array of cell-centre coords
  !> @param[in]   cell_lid           The local id of the requested cell
  !> @param[out]  cell_centre_coords A three-element array containing the
  !>                                 coordinates of a the centre of a single
  !>                                 cell in the mesh object
  !============================================================================
  subroutine get_cell_centre_coords(self, cell_lid, cell_centre_coords)

    implicit none
    class(mesh_type), intent(in)  :: self
    integer(i_def),   intent(in)  :: cell_lid
    real(r_def),      intent(out) :: cell_centre_coords(:)

    cell_centre_coords(:) = self%cell_centre_coords(:,cell_lid)

  end subroutine get_cell_centre_coords

  !> @details This subroutine returns 3-element array of vertex coords for
  !>          each vertex on the request local cell id. Coords are in
  !>          cartesian coords [x,y,z] in [m] and in same order as the
  !>          vertex on cell connectivity array
  !> @param[in]  cell_lid     The local id of the requested cell
  !> @param[out  cell_coords  A 2-dimensional array which contains
  !>                          the cartesian coordinates of each vertex
  !>                          on the requested cell_lid. The returned
  !>                          array will have dimensions of
  !>                          [3, nVertices on cell]
  !============================================================================
  subroutine get_cell_coords(self, cell_lid, cell_coords)

    ! Returns 3-element array of vertex coords for each
    ! vertex on the request local cell id. Coords are
    ! in cartesian coords [x,y,z] in [m] and in same order
    ! as the vertex on cell connectivity array

    implicit none
    class(mesh_type), intent(in)  :: self
    integer(i_def),   intent(in)  :: cell_lid
    real(r_def),      intent(out) :: cell_coords(:,:)

    integer(i_def) :: ivert, vert_lid

    do ivert=1, self%nverts_per_cell
      vert_lid = self%vert_on_cell(ivert, cell_lid)
      call self%get_vert_coords(vert_lid, cell_coords(:,ivert))
    end do

  end subroutine get_cell_coords


  !> @details This subroutine returns 3-element array of vertex coords
  !>          for a vertical cell column in the local mesh. Any local
  !>          cell id within the column can be provided.
  !> @param[in]  cell_lid      The local id of the requested cell
  !> @param[out] column_coords A 3-dimensional array which contains the
  !>                           cartesian coordinates of each vertex on the
  !>                           column of 3D-cells which include the requested
  !>                           cell_lid. The returned array will have
  !>                           dimensions of [3, nVertices on cell, nlayers]
  !============================================================================
  subroutine get_column_coords(self, cell_lid, column_coords)

    ! Returns 3-element array of vertex coords for a vertical
    ! cell column in the local mesh. Any local cell id within
    ! the column can be provided.

    implicit none
    class(mesh_type), intent(in)  :: self
    integer(i_def),   intent(in)  :: cell_lid
    real(r_def),      intent(out) :: column_coords(:,:,:)

    integer(i_def) :: base_id, k, icell

    ! Get the cell id of the cell at the base of the column containing input cell
    base_id = modulo(cell_lid,self%ncells_2d_with_ghost)
    if (base_id == 0) base_id = self%ncells_2d_with_ghost

    ! Get the coordinates of each cell in the column
    do k=1, self%nlayers
      icell = base_id + (k-1)*self%ncells_2d_with_ghost
      call self%get_cell_coords(icell,column_coords(:,:,k))
    end do

  end subroutine get_column_coords

  !> @details This function returns the number of 3d-cell layers in the mesh
  !>          object
  !> @return  Number of 3d-cell vertical layers in the mesh object
  !============================================================================
  function get_nlayers(self) result (nlayers)

    ! This function returns the number of 3d-cell layers
    ! in the mesh object

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nlayers

    nlayers = self%nlayers

  end function get_nlayers


  !> @details This function returns the number of
  !>          vertices per 3d-cell on this mesh
  !> @return  Number of vertices per 3d-cell on mesh
  !============================================================================
  function get_nverts_per_cell(self) result (nverts_per_cell)

    ! Returns number of vertices per 3d-cell on this mesh

    implicit none
    class (mesh_type), intent(in) :: self
    integer(i_def)                :: nverts_per_cell

    nverts_per_cell = self%nverts_per_cell

  end function get_nverts_per_cell

  !> @details This function returns the number of
  !>          vertices per 2d-cell/face on this mesh
  !> @return  Number of vertices per 2d-cell/face on mesh
  !============================================================================
  function get_nverts_per_cell_2d(self) result (nverts_per_cell_2d)

    ! Returns number of vertices per 2d-cell on this mesh

    implicit none
    class (mesh_type), intent(in) :: self
    integer(i_def)                :: nverts_per_cell_2d

    nverts_per_cell_2d = self%nverts_per_2d_cell

  end function get_nverts_per_cell_2d

  !> @details This function returns the number of
  !>          vertices per edge on this mesh
  !> @return  Number of vertices per edge on mesh
  !============================================================================
  function get_nverts_per_edge(self) result (nverts_per_edge)

    ! Returns number of vertices per edge on this mesh

    implicit none
    class (mesh_type), intent(in) :: self
    integer(i_def)                :: nverts_per_edge

    nverts_per_edge = self%nverts_per_edge

  end function get_nverts_per_edge


  !> @details This function returns the number of
  !>          edges per 3d-cell on this mesh
  !> @return  Number of edges per 3d-cell on mesh
  !============================================================================
  function get_nedges_per_cell(self) result (nedges_per_cell)

    ! Returns number of edges per 3d-cell on this mesh

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nedges_per_cell

    nedges_per_cell = self%nedges_per_cell

  end function get_nedges_per_cell

  !> @details This function returns the number of
  !>          edges per 2d-cell/face on this mesh
  !> @return  Number of edges per 2d-cell/face on mesh
  !============================================================================
  function get_nedges_per_cell_2d(self) result (nedges_per_cell_2d)

    ! Returns number of edges per 2d-cell on this mesh

    implicit none
    class (mesh_type), intent(in) :: self
    integer(i_def)                :: nedges_per_cell_2d

    nedges_per_cell_2d = self%nedges_per_2d_cell

  end function get_nedges_per_cell_2d

  !> @details This function returns the number
  !>          of faces per 3d-cell on this et-mesh
  !> @return  Number of faces per 3d-cell on mesh
  !============================================================================
  function get_nfaces_per_cell(self) result (nfaces_per_cell)

    ! Returns number of faces per 3d-cell on this mesh

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nfaces_per_cell

    nfaces_per_cell = self%nfaces_per_cell

  end function get_nfaces_per_cell


  !> @details Returns local cell id of adjacent cell on the specified face
  !>          (iface) of the given cell (icell)
  !> @param[in] iface         The index (on the known cell with local id,
  !>                          @c cell_lid ) of the face common to both cells
  !> @param[in] cell_lid      The local id of the known cell
  !> @return                  The local id of cell adjacent to the
  !>                          known cell  ( @c cell_lid ) with the
  !>                          common face ( @c iface )
  !============================================================================
  function get_cell_next(self, iface, cell_lid) result (cell_next_lid)

    ! Returns local cell id of adjacent cell on the
    ! specified face (iface) of the given cell (cell_lid)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: iface      ! Index of face required
    integer(i_def),   intent(in) :: cell_lid   ! Local cell id
    integer(i_def)               :: cell_next_lid

    cell_next_lid = self%cell_next(iface, cell_lid)

  end function get_cell_next


  !> @details This function returns the local face id on local cell
  !> @param [in]  iface     The index face of interest
  !> @param [in]  icell     The local id of the cell which the
  !>                        face is a member of
  !> @return                The local id of face on index iface
  !>                        of cell with local id icell
  !============================================================================
  function get_face_on_cell(self, iface, icell) result (face_lid)

    ! Returns local face id on local cell

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: iface   ! Index of face required
    integer(i_def),   intent(in) :: icell   ! Local cell id
    integer(i_def)               :: face_lid

    face_lid = self%face_on_cell(iface, icell)

  end function get_face_on_cell


  !> @details This function returns the local edge id on the local cell
  !> @param [in] iedge    The index of the edge whose local id is requested
  !> @param [in] icell    The local id of the cell on which the edge is located
  !> @return              Local id of edge on index iedge of cell with
  !>                      local id icell
  !============================================================================
  function get_edge_on_cell(self, iedge, icell) result (edge_lid)

    ! Returns local edge id on local cell

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: iedge   ! Index of edge required
    integer(i_def),   intent(in) :: icell   ! Local cell id
    integer(i_def)               :: edge_lid

    edge_lid = self%edge_on_cell(iedge, icell)

  end function get_edge_on_cell


  !> @details This function returns the local vertex id on the local cell
  !> @param [in] ivert    The index of the vertex whose local id is requested
  !> @param [in] icell    The local id of the cell on which the vertex
  !>                      is located
  !> @return              Local id of vertex on index ivert of cell with
  !>                      local id icell
  !============================================================================
  function get_vert_on_cell(self, ivert, icell) result (vert_lid)

    ! Returns local vertex id on local cell

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: ivert   ! Index of vertex required
    integer(i_def),   intent(in) :: icell   ! Local cell id
    integer(i_def)               :: vert_lid

    vert_lid = self%vert_on_cell(ivert, icell)

  end function get_vert_on_cell


  !> @details This function returns the number of cells in the horizontal
  !>          (i.e. the number of cells in the partition), @b excluding any
  !>          ghost cells.
  !> @return  Number of cells on a horizontal layer.
  !============================================================================
  function get_ncells_2d(self) result (ncells_2d)

    ! This function returns the number of cells in the horizontal
    ! (i.e. the number of cells in the partition), EXCLUDING any
    ! ghost cells.

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def) :: ncells_2d

    ncells_2d = self%ncells_2d

  end function get_ncells_2d


  !> @details This function returns the number of cells in the horizontal
  !>          (i.e. the number of cells in the partition), @b including any
  !>          ghost cells.
  !> @return  Number of cells on a horizontal layer
  !>         @b plus ghost cells.
  !============================================================================
  function get_ncells_2d_with_ghost(self) result (ncells_2d_with_ghost)

    ! This function returns the number of cells in the horizontal
    ! (i.e. the number of cells in the partition), INCLUDING any
    ! ghost cells.

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def) :: ncells_2d_with_ghost

    ncells_2d_with_ghost = self%ncells_2d_with_ghost

  end function get_ncells_2d_with_ghost


  !> @details This function returns the number of edges on one horizontal level
  !>          of the mesh. This is the same as the number of edges on the local
  !>          partition.
  !> @return  Number of edges on one horizontal level in the
  !>                   mesh object i.e. one edge thick
  !============================================================================
  function get_nedges_2d(self) result (nedges_2d)

    ! Returns total number of horizontal edges on each 2d-level
    ! of this mesh. This is the same as the number of edges on the
    ! local partition.

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nedges_2d

    nedges_2d = self%nedges_2d

  end function get_nedges_2d


  !> @details This function returns the number of vertices on one horizontal
  !>          level of the mesh. This is the same as the number of vertices
  !>          on the local partition.
  !> @return  Number of vertices on horizontal layer in the
  !>                   mesh object i.e. one vertex in thick
  !============================================================================
  function get_nverts_2d(self) result (nverts_2d)

    ! Returns the number of vertices on one horizontal level
    ! of the mesh. This is the same as the number of vertices
    ! on the local partition.

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nverts_2d

    nverts_2d = self%nverts_2d

  end function get_nverts_2d


  !> @details This function returns the number of 3d-cells in the local 3d-mesh.
  !> @return  Total number of 3d-cells in the mesh object
  !============================================================================
  function get_ncells(self) result (ncells)

    ! Returns total number of 3d-cells in this mesh

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: ncells

    ncells = self%ncells

  end function get_ncells

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of vertices in the local 3d-mesh.
  !>
  !> @return Total number of vertices in the mesh object.
  !>
  function get_nverts(self) result (nverts)

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def) :: nverts

    nverts = self%nverts

  end function get_nverts


  !> @details This function returns the number of edges in the local 3d-mesh.
  !> @return  Total number of edges in the mesh object
  !============================================================================
  function get_nedges(self) result (nedges)

    ! Returns total number of edges in this 3d-mesh

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nedges

    nedges = self%nedges

  end function get_nedges

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of faces in the local 3d-mesh.
  !>
  !> @return Total number of faces in the mesh object.
  !>
  function get_nfaces(self) result (nfaces)

    implicit none

    class(mesh_type), intent(in) :: self
    ! Return variable
    integer(i_def) :: nfaces

    nfaces = self%nfaces

  end function get_nfaces

  !> @details Returns the height above surface of the top of the mesh domain
  !> @return  Height of top of mesh
  !============================================================================
  function get_domain_top(self) result (domain_top)

    ! Returns top of model height above surface

    implicit none
    class (mesh_type), intent(in) :: self
    real  (r_def)                 :: domain_top

    domain_top = self%domain_depth

  end function get_domain_top

  !> @details Returns the identifier for the type of extrusion the mesh has
  !> @return  Extrusion ID
  !============================================================================
  function get_extrusion_id(self) result (extrusion_id)

    implicit none
    class (mesh_type), intent(in) :: self
    integer(i_def)                :: extrusion_id

    extrusion_id = self%extrusion_id

  end function get_extrusion_id

  !> @details This functions returns an array of 3d-layer thicknesses in
  !>          metres
  !> @param[out]  dz  Vertical thickness of layers in [m], array of
  !>                  length @c nlayers . The number of layers in the mesh
  !>                  object can be obtained using the @c get_nlayers
  !>                  type-bound function.
  !============================================================================
  subroutine get_dz(self,dz)

    ! Returns array of vertical layer thicknesses (in metres) for this 3d-mesh

    implicit none
    class (mesh_type), intent(in) :: self
    real(r_def),      intent(out) :: dz(:)

    ! Get the thickness of layers
    dz(:) = self%dz(:)

  end subroutine get_dz


  !> @param[out] eta Array of dimensions (0:nlayers) of non-dimensional
  !>                 vertical coordinate normalised using the @c domain_height
  !============================================================================
  subroutine get_eta(self,eta)

    ! Returns non-dimensional vertical coordinate eta[0,1]

    implicit none
    class (mesh_type), intent(in) :: self
    real(r_def),      intent(out) :: eta(:)

    ! Get the thickness of layers
    eta(:) = self%eta(:)

  end subroutine get_eta

  !> @brief   Return global domain extents
  !> @return  domain <domain_type> holding information on global domain.
  !============================================================================
  function get_domain(self) result( domain )

    ! Returns local mesh domain limits as a domain_type.

    implicit none
    class(mesh_type), intent(in) :: self
    type(domain_type)            :: domain

    domain = self%domain

  end function get_domain

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the cell which owns a vertex.
  !>
  !> @param[in] vertex_index  Index of the vertex entity on the cell with
  !>                          local_id of cell_lid.
  !> @param[in] cell_lid      Local cell id on which the vertex entity is
  !>                          associated with.
  !>
  !> @return Local ID of the cell that "owns" the vertex entity.
  !>
  function get_vertex_cell_owner( self, vertex_index, cell_lid ) &
                                  result (cell_owner)

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: vertex_index
    integer(i_def),   intent(in) :: cell_lid
    integer(i_def)               :: cell_owner

    cell_owner = self%vert_cell_owner( vertex_index, cell_lid )

  end function get_vertex_cell_owner

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the cell which owns an edge.
  !>
  !> @param[in] edge_index  The index of the edge entity on the cell with
  !>                        local_id of cell_lid.
  !> @param[in] cell_lid    The local cell id on which the edge entity is
  !>                        associated with.
  !>
  !> @return Local ID of the cell that "owns" the edge entity.
  !>
  function get_edge_cell_owner( self, edge_index, cell_lid ) &
                                result (cell_owner)

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: edge_index
    integer(i_def),   intent(in) :: cell_lid
    integer(i_def)               :: cell_owner

    cell_owner = self%edge_cell_owner( edge_index, cell_lid )

  end function get_edge_cell_owner

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Finds out if a vertix is in the local partition.
  !>
  !> @param[in] vertex_index  The index of the vertex entity on the cell with
  !>                          local_id of cell_lid.
  !> @param[in] cell_lid      The local cell id on which the edge entity is
  !>                          associated with.
  !>
  !> @return True if the vertex is owned by the local partition.
  !>
  function is_vertex_owned( self, vertex_index, cell_lid ) result (owned)

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: vertex_index
    integer(i_def),   intent(in) :: cell_lid
    logical(l_def)               :: owned

    owned = .false.
    if ( self%vertex_ownership( vertex_index, cell_lid ) == &
         global_mpi%get_comm_rank() ) owned = .true.

  end function is_vertex_owned

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Finds out if an edge is in the local partition.
  !>
  !> @param[in] edge_index  The index of the edge entity on the cell with
  !>                        local_id of cell_lid.
  !> @param[in] cell_lid    The local cell id which the edge entity is
  !>                        associated with.
  !>
  !> @return True if the edge is owned by the local partition.
  !>
  function is_edge_owned( self, edge_index, cell_lid ) result (owned)

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: edge_index
    integer(i_def),   intent(in) :: cell_lid
    logical(l_def)               :: owned

    owned = .false.
    if ( self%edge_ownership( edge_index, cell_lid ) == &
         global_mpi%get_comm_rank() ) owned = .true.

  end function is_edge_owned

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Finds out if a cell is in the local partition.
  !>
  !> @param[in] cell_lid  The local cell id on the local partition.
  !>
  !> @return True if the cell is owned by the local partition.
  !>
  function is_cell_owned( self, cell_lid ) result (owned)

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: cell_lid
    logical(l_def)               :: owned

    owned = .false.
    if ( self%local_mesh%get_cell_owner(cell_lid) == &
         global_mpi%get_comm_rank())    owned = .true.

  end function is_cell_owned

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Returns the number of edges owned by the local partition.
  !>
  !> @return Number of edges owned by the local partition.
  !>
  function get_num_edges_owned_2d( self ) result (num_owned_edges)

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def)               :: num_owned_edges

    integer(i_def) :: icell
    integer(i_def) :: iedge

    num_owned_edges = 0
    do icell=1, self%get_last_edge_cell()
      do iedge=1, self%get_nedges_per_cell_2d()
        if ( self%get_edge_cell_owner( iedge, icell ) == icell) &
                                    num_owned_edges = num_owned_edges + 1
      end do
    end do

  end function get_num_edges_owned_2d

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Returns the number of vertices owned by the local partition.
  !>
  !> @return Number of vertices owned by the local partition.
  !>
  function get_num_verts_owned_2d( self ) result (num_owned_verts)

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def)               :: num_owned_verts

    integer(i_def) :: icell
    integer(i_def) :: ivert

    num_owned_verts = 0
    do icell=1, self%get_last_edge_cell()
      do ivert=1, self%get_nverts_per_cell_2d()
        if ( self%get_vertex_cell_owner( ivert, icell ) == icell) &
                                    num_owned_verts = num_owned_verts + 1
      end do
    end do

  end function get_num_verts_owned_2d

  !> Returns the maximum depth of the inner halos from the local partition
  !> @return inner_halo_depth The maximum depth of the inner halo cells
  !============================================================================
  function get_inner_depth( self ) result ( inner_depth )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def) :: inner_depth

    inner_depth = self%local_mesh%get_inner_depth()

  end function get_inner_depth

  !> Returns the total number of inner halo cells in a particular depth of
  !> inner halo in a 2d slice from the local partition
  !> @param[in] depth The depth of the inner halo being queried
  !> @return inner_halo_cells The total number of inner halo cells of the
  !>                          particular depth on the local partition
  !============================================================================
  function get_num_cells_inner( self, depth ) result ( inner_cells )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def)             :: inner_cells

    if( depth > self%get_inner_depth() .or. depth < 1 )then
      inner_cells = 0
    else
      inner_cells = self%local_mesh%get_num_cells_inner(depth)
    end if

  end function get_num_cells_inner

  !> @brief  Gets the index of the last cell in an inner halo
  !> @details Returns the index of the last cell in a particular depth
  !>          of inner halo in a 2d slice on the local partition
  !> @param[in] depth The depth of the inner halo being queried
  !> @return last_inner_cell The index of the last cell in the particular depth
  !>         of inner halo on the local partition
  !============================================================================
  function get_last_inner_cell( self, depth ) result ( last_inner_cell )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def)             :: last_inner_cell

    if( depth > self%get_inner_depth() .or. depth < 0 )then
      last_inner_cell = 0
    else if( depth == 0 )then
      ! The zeroth depth inner halo has no size, so its last cell is in the
      ! same place as the inner halo before it in memory: inner(1)
      last_inner_cell = self%local_mesh%get_last_inner_cell(1)
    else
      last_inner_cell = self%local_mesh%get_last_inner_cell(depth)
    end if

  end function get_last_inner_cell

  !> @brief  Gets the number of cells of a given colour up to the specified
  !>         inner halo
  !> @param[in] colour Colour of cells to return number of
  !> @param[in] depth The depth of the halo being queried
  !> @return last_inner_cell The number of cells of the requested colour
  !>                         in the partition, up to the specified inner halo
  function get_last_inner_cell_per_colour( self, colour, depth ) &
                                          result ( last_inner_cell )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def), intent(in) :: colour

    character(len=*),parameter :: function_name = &
                                    'get_last_inner_cell_per_colour'
    integer(i_def)             :: last_inner_cell

    ! Check arguments, which will abort if out of bounds
    call bounds_check (self, function_name, colour=colour, depth=depth )
    if(depth == 0)then
      ! The zeroth depth inner halo has no size, so its last cell is in the
      ! same place as the inner halo before it in memory: inner(1)
      last_inner_cell = self%last_inner_cell_per_colour(colour, 1)
    else
      last_inner_cell = self%last_inner_cell_per_colour(colour, depth)
    end if

  end function get_last_inner_cell_per_colour

  !> @brief  Gets the number of cells of all colours for each inner halo
  !> @return all_last_inner_cells An array holding the number of cells of each
  !>         colour in the partition, for each depth of inner halo
  function get_last_inner_cell_all_colours( self ) &
                                           result ( all_last_inner_cells )
    implicit none

    class(mesh_type), intent(in) :: self

    character(len=*),parameter :: function_name = &
                                    'get_last_inner_cell_all_colours'
    integer(i_def), allocatable    :: all_last_inner_cells(:,:)

    allocate( all_last_inner_cells( self%ncolours, self%get_inner_depth() ) )

    all_last_inner_cells = self%last_inner_cell_per_colour

  end function get_last_inner_cell_all_colours

  !> @brief  Gets the number of tiles of a given colour up to the specified
  !>         inner halo
  !> @param[in] colour Colour of tiles to return number of
  !> @param[in] depth  Depth of the inner halo being queried
  !> @return last_inner_tile The number of tiles of the requested colour
  !>                         in the partition, up to the specified inner halo
  function get_last_inner_tile_per_colour( self, colour, depth ) &
                                          result ( last_inner_tile )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), intent(in)   :: colour
    integer(i_def), intent(in)   :: depth
    integer(i_def)               :: last_inner_tile

    last_inner_tile = self%last_inner_tile_per_colour( colour, depth )

  end function get_last_inner_tile_per_colour

  !> @brief  Gets the number of tiles for all colours and halo depths
  !> @return last_inner_tiles The number of tiles of all colours in the
  !>                          partition for all inner halos
  function get_last_inner_tile_all_colours( self ) &
                                           result ( last_inner_tiles )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), allocatable  :: last_inner_tiles(:,:)

    allocate( last_inner_tiles, source=self%last_inner_tile_per_colour )

  end function get_last_inner_tile_all_colours

  !> @brief  Gets the number of cells in a given coloured tile up to the
  !>         specified inner halo
  !> @param[in] colour Colour of tiles to return cell number of
  !> @param[in] tile   Tile to return cell number of
  !> @param[in] depth  Depth of the inner halo being queried
  !> @return last_inner_cell The number of cells in the requested coloured tile
  !>                         in the partition, up to the specified inner halo
  function get_last_inner_cell_per_colour_and_tile( self, colour, tile, depth ) &
                                                   result ( last_inner_cell )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), intent(in)   :: colour
    integer(i_def), intent(in)   :: tile
    integer(i_def), intent(in)   :: depth
    integer(i_def)               :: last_inner_cell

    last_inner_cell = self%last_inner_cell_per_coloured_tile( colour, tile, &
                                                              depth )

  end function get_last_inner_cell_per_colour_and_tile

  !> @brief  Gets the number of cells for all colours, tiles, and inner halos
  !> @return last_inner_tiles The number of cells for all colours, tiles, and
  !>                          inner halos
  function get_last_inner_cell_all_colours_all_tiles( self ) &
                                                     result ( last_inner_cells )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), allocatable  :: last_inner_cells(:,:,:)

    allocate( last_inner_cells, source=self%last_inner_cell_per_coloured_tile )

  end function get_last_inner_cell_all_colours_all_tiles

  !> Get the number of edge cells in the local partition
  !> @return edge_cells The total number of edge cells on the
  !> local partition
  !============================================================================
  function get_num_cells_edge( self ) result ( edge_cells )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def) :: edge_cells

    edge_cells = self%local_mesh%get_num_cells_edge()

  end function get_num_cells_edge

  !> @brief  Gets the index of the last edge cell in a 2d slice on the local
  !>         partition
  !> @return last_edge_cell The index of the last of "edge" cell on the local
  !>         partition
  !============================================================================
  function get_last_edge_cell( self ) result ( last_edge_cell )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def) :: last_edge_cell

    last_edge_cell = self%local_mesh%get_last_edge_cell()

  end function get_last_edge_cell

  !> @brief  Gets the number of cells of a given colour in the domain of the
  !>         the mesh excluding halos
  !> @param[in] colour Colour of cells to return cell count number of
  !> @return last_edge_cell The number of cells of a given colour in a region up
  !>         to the last edge cell
  !============================================================================
  function get_last_edge_cell_per_colour( self, colour ) &
                                        result ( last_edge_cell )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: colour

    character(len=*),parameter :: function_name = &
                                    'get_last_edge_cell_per_colour'
    integer(i_def) :: last_edge_cell

    ! Check arguments, which will abort if out of bounds
    call bounds_check (self, function_name, colour=colour )
    last_edge_cell = self%last_edge_cell_per_colour(colour)

  end function get_last_edge_cell_per_colour

  !> @brief  Gets the number of cells of a each colour in the domain of the
  !>         the mesh excluding halos
  !> @return all_last_edge_cells An array of cell counts up to the last of "edge"
  !>         cell on the local partition for each colour
  !============================================================================
  function get_last_edge_cell_all_colours( self ) &
                                        result ( all_last_edge_cells )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), allocatable :: all_last_edge_cells(:)

    allocate( all_last_edge_cells( self%ncolours ) )

    all_last_edge_cells = self%last_edge_cell_per_colour

  end function get_last_edge_cell_all_colours

  !> @brief  Gets the number of tiles of a given colour in the partition,
  !>         excluding halos
  !> @param[in] colour Colour of tiles to return number of
  !> @return last_edge_tile The number of tiles of a given colour in the
  !>                        partition up to the last edge cell
  !============================================================================
  function get_last_edge_tile_per_colour( self, colour ) &
                                         result ( last_edge_tile )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), intent(in)   :: colour
    integer(i_def)               :: last_edge_tile

    ! Edge cells and inner halo cells are parts of the same tiles
    last_edge_tile = self%last_edge_tile_per_colour( colour )

  end function get_last_edge_tile_per_colour

  !> @brief  Gets the number of tiles for all colours in the partition,
  !>         excluding halos
  !> @return last_edge_tiles The number of edge tiles for all colours
  function get_last_edge_tile_all_colours( self ) &
                                          result ( last_edge_tiles )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), allocatable  :: last_edge_tiles(:)

    allocate( last_edge_tiles, source=self%last_edge_tile_per_colour )

  end function get_last_edge_tile_all_colours

  !> @brief  Gets the number of cells in a given coloured tile in the partition
  !>         excluding halos
  !> @param[in] colour Colour of tiles to return cell number of
  !> @param[in] tile   Tile to return cell number of
  !> @return last_edge_cell The number of cells in a given coloured tile in
  !>                        the partition up to the last edge cell
  !============================================================================
  function get_last_edge_cell_per_colour_and_tile( self, colour, tile ) &
                                                  result ( last_edge_cell )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), intent(in)   :: colour
    integer(i_def), intent(in)   :: tile
    integer(i_def)               :: last_edge_cell

    last_edge_cell = self%last_edge_cell_per_coloured_tile( colour, tile )

  end function get_last_edge_cell_per_colour_and_tile

  !> @brief  Gets the number of edge cells for all colours and tiles
  !> @return last_inner_tiles The number of edge cells for all colours and tiles
  function get_last_edge_cell_all_colours_all_tiles( self ) &
                                                    result ( last_edge_cells )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), allocatable  :: last_edge_cells(:,:)

    allocate( last_edge_cells, source=self%last_edge_cell_per_coloured_tile )

  end function get_last_edge_cell_all_colours_all_tiles

  !> @details Returns the maximum depth of the halo on the local partition
  !> @return  The maximum depth of halo cells
  !============================================================================
  function get_halo_depth( self ) result ( halo_depth )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def)               :: halo_depth

    halo_depth = self%local_mesh%get_halo_depth()

  end function get_halo_depth


  !> @details Returns the total number of halo cells in a particular depth
  !>          of halo in a 2d slice on the local partition
  !> @param[in] depth       The depth of the halo being queried
  !> @return                The total number of halo cells of the particular
  !>                        depth on the local partition
  !============================================================================
  function get_num_cells_halo( self, depth ) result ( halo_cells )

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: depth
    integer(i_def)               :: halo_cells

    if (depth > self%get_halo_depth() .or. depth < 1) then
      halo_cells = 0
    else
      halo_cells = self%local_mesh%get_num_cells_halo(depth)
    end if

  end function get_num_cells_halo

  !> @brief  Gets the index of the last cell in the specified halo
  !> @details Returns the index of the last cell in a particular depth
  !>          of halo in a 2d slice on the local partition
  !> @param[in] depth The depth of the halo being queried
  !> @return last_halo_cell The index of the last cell in the particular depth
  !>         of halo on the local partition
  !============================================================================
  function get_last_halo_cell_any( self, depth ) result ( last_halo_cell )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth

    character(len=*),parameter :: function_name = &
                                    'get_last_halo_cell_any'
    integer(i_def)             :: last_halo_cell

    ! Check arguments, which will abort if out of bounds
    call bounds_check (self, function_name, depth=depth )
    if (depth == 0) then
      ! The zeroth depth halo has no size, so its last cell is in the
      ! same place as the last edge cell
      last_halo_cell = self%local_mesh%get_last_edge_cell()
    else
      last_halo_cell = self%local_mesh%get_last_halo_cell(depth)
    end if

  end function get_last_halo_cell_any

  !> @brief  Gets the number of cells of a given colour up to the specified halo
  !> @param[in] colour Colour of cells to return number of
  !> @param[in] depth The depth of the halo being queried
  !> @return ncells_colour The number of this coloured cells in the partition up
  !>         to the specified halo
  !============================================================================
  function get_last_halo_cell_per_colour_any( self, colour, depth ) &
                                            result ( ncells_colour )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def), intent(in) :: colour

    character(len=*),parameter :: function_name = &
                                    'get_last_halo_cell_per_colour_any'
    integer(i_def)             :: ncells_colour

    ! Check arguments, which will abort if out of bounds
    call bounds_check (self, function_name, colour=colour, depth=depth )
    if (depth == 0) then
      ! The zeroth depth halo has no size, so its last cell is in the
      ! same place as the last edge cell
      ncells_colour = self%get_last_edge_cell_per_colour(colour)
    else
      ncells_colour = self%last_halo_cell_per_colour(colour, depth)
    end if

  end function get_last_halo_cell_per_colour_any

  !> @brief  Gets the number of cells of each colour for all halos
  !> @return ncells_colour The number cells in the partition up
  !>         to each halo depth for all colours
  !============================================================================
  function get_last_halo_cell_all_colours( self ) &
                                          result ( ncells )
    implicit none

    class(mesh_type), intent(in) :: self

    character(len=*),parameter :: function_name = &
                                    'get_last_halo_cell_all_colours'
    integer(i_def),allocatable :: ncells(:,:)

    allocate( ncells( self%ncolours, self%get_halo_depth() ) )

    ncells = self%last_halo_cell_per_colour

  end function get_last_halo_cell_all_colours

  !> @brief  Gets the number of cells of a given colour in the partition
  !> @param[in] colour Colour of cells to return number of
  !> @return ncells_colour The number of this coloured cells in the partition
  !============================================================================
  function get_last_halo_cell_per_colour_deepest( self, colour) &
                                            result ( ncells_colour )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: colour
    character(len=*),parameter :: function_name = &
                                    'get_last_halo_cell_per_colour_deepest'

    integer(i_def)             :: ncells_colour

    ! Check arguments, which will abort if out of bounds
    call bounds_check (self, function_name, colour=colour )
    ncells_colour = self%ncells_per_colour(colour)

  end function get_last_halo_cell_per_colour_deepest

  !> @brief  Gets the number of cells of each colour in the partition
  !> @return ncells_colour The number of cells in the partition for each colour
  !============================================================================
  function get_last_halo_cell_all_colours_deepest( self ) &
                                            result ( ncells_colour )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), allocatable    :: ncells_colour(:)

    allocate( ncells_colour( self%ncolours ) )

    ncells_colour = self%ncells_per_colour

  end function get_last_halo_cell_all_colours_deepest

  !> @brief  Gets the number of tiles of a given colour up to the specified
  !>         outer halo
  !> @param[in] colour Colour of tiles to return number of
  !> @param[in] depth  Depth of the outer halo being queried
  !> @return ntiles_colour The number of coloured tiles in the partition up
  !>                       to the specified outer halo
  !============================================================================
  function get_last_halo_tile_per_colour_any( self, colour, depth ) &
                                             result ( ntiles_colour )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), intent(in)   :: colour
    integer(i_def), intent(in)   :: depth
    integer(i_def)               :: ntiles_colour

    ntiles_colour = self%last_halo_tile_per_colour( colour, depth )

  end function get_last_halo_tile_per_colour_any

  !> @brief  Gets the number of tiles of a given colour in the partition
  !> @param[in] colour Colour of tiles to return number of
  !> @return ntiles_colour The number of coloured tiles in the partition
  !============================================================================
  function get_last_halo_tile_per_colour_deepest( self, colour ) &
                                                 result ( ntiles_colour )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), intent(in)   :: colour
    integer(i_def)               :: ntiles_colour

    ntiles_colour = self%ntiles_per_colour(colour)

  end function get_last_halo_tile_per_colour_deepest


  !> @brief  Gets the number of tiles of each colour for all outer halos
  !> @return ntiles_colour The number tiles in the partition up to each halo
  !>                       depth for all colours
  !============================================================================
  function get_last_halo_tile_all_colours( self ) &
                                          result ( ntiles )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), allocatable  :: ntiles(:,:)

    allocate( ntiles, source=self%last_halo_tile_per_colour )

  end function get_last_halo_tile_all_colours

  !> @brief  Gets the number of tiles in the partition for all colours
  !> @return ntiles The number tiles in the partition for all colours
  !============================================================================
  function get_last_halo_tile_all_colours_deepest( self ) &
                                                  result ( ntiles )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), allocatable  :: ntiles(:)

    allocate( ntiles, source=self%ntiles_per_colour )

  end function get_last_halo_tile_all_colours_deepest

  !> @brief  Gets the number of cells in a given coloured tile up to the
  !>         specified outer halo
  !> @param[in] colour Colour of tiles to return cell number of
  !> @param[in] tile   Tile being queried
  !> @param[in] depth  Depth of the outer halo being queried
  !> @return ncells_coloured_tile The number of cells in a coloured tile in the
  !>                              partition up to the specified outer halo
  !============================================================================
  function get_last_halo_cell_per_colour_and_tile_any( self, colour, tile, &
                                                       depth )             &
                                          result ( ncells_coloured_tile )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), intent(in)   :: colour
    integer(i_def), intent(in)   :: tile
    integer(i_def), intent(in)   :: depth
    integer(i_def)               :: ncells_coloured_tile

    ncells_coloured_tile = self%last_halo_cell_per_coloured_tile( colour, &
                                                                  tile,   &
                                                                  depth )

  end function get_last_halo_cell_per_colour_and_tile_any

  !> @brief  Gets the number of cells in a given coloured tile in the partition
  !> @param[in] colour Colour of tiles to return cell number of
  !> @param[in] tile   Tile being queried
  !> @return ncells_coloured_tile The number of cells in a coloured tile in the
  !>                              partition
  !============================================================================
  function get_last_halo_cell_per_colour_and_tile_deepest( self, colour, tile ) &
                                                result ( ncells_coloured_tile )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), intent(in)   :: colour
    integer(i_def), intent(in)   :: tile
    integer(i_def)               :: ncells_coloured_tile

    ncells_coloured_tile = self%ncells_per_coloured_tile( colour, tile )

  end function get_last_halo_cell_per_colour_and_tile_deepest

  !> @brief  Gets the number of cells in all coloured tiles for all halos
  !> @return ncells_coloured_tile The number cells in all coloured tiles for
  !>                              all halos
  !============================================================================
  function get_last_halo_cell_all_colours_all_tiles( self ) &
                                                    result ( ncells )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def),allocatable   :: ncells(:,:,:)

    allocate( ncells, source= self%last_halo_cell_per_coloured_tile )

  end function get_last_halo_cell_all_colours_all_tiles

  !> @brief  Gets the number of cells in all coloured tiles in the partition
  !> @return ncells The number cells in all coloured tiles in the partition
  !============================================================================
  function get_last_halo_cell_all_colours_all_tiles_deepest( self ) &
                                                            result ( ncells )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def),allocatable   :: ncells(:,:)

    allocate( ncells, source=self%ncells_per_coloured_tile )

  end function get_last_halo_cell_all_colours_all_tiles_deepest


  !> @brief  Initialises the index of the last cell in various specified regions
  !>         for each colour
  !> @details In the PSy layer, we need to loop over various subsets of the mesh
  !>         such as up to a given halo depth (inner or outer) or up to the last
  !>         edge cell. In order for this to work with colouring, we need to
  !>         know the number of cells of each colour in each subset.
  !============================================================================
  subroutine init_last_cell_per_colour( self )

    implicit none

    class(mesh_type), intent(inout) :: self

    integer(i_def)             :: depth
    integer(i_def)             :: colour
    integer(i_def)             :: cell
    integer(i_def)             :: last_cell

    ! Do calculations for the halos
    if (self%get_halo_depth() > 0) then
      allocate(self%last_halo_cell_per_colour(self%ncolours, self%get_halo_depth()))
      do depth = 1, self%get_halo_depth()
        last_cell = self%get_last_halo_cell(depth)
        do colour = 1, self%ncolours
          do cell = self%ncells_per_colour(colour),1,-1
            if (self%cells_in_colour(colour, cell) <= last_cell) exit
          end do
          self%last_halo_cell_per_colour(colour, depth) = cell
        end do
      end do
    end if

    ! Do calculations for the inner halos
    if (self%get_inner_depth() > 0) then
      allocate(self%last_inner_cell_per_colour(self%ncolours, self%get_inner_depth()))
      do depth = 1, self%get_inner_depth()
        last_cell = self%get_last_inner_cell(depth)
        do colour = 1, self%ncolours
          do cell = self%ncells_per_colour(colour),1,-1
            if (self%cells_in_colour(colour,cell) <= last_cell) exit
          end do
          self%last_inner_cell_per_colour(colour, depth) = cell
        end do
      end do
    end if

    ! Do calculations for the range excluding halos
    allocate(self%last_edge_cell_per_colour(self%ncolours))
    last_cell = self%get_last_edge_cell()

    do colour = 1, self%ncolours
      do cell = self%ncells_per_colour(colour),1,-1
        if (self%cells_in_colour(colour,cell) <= last_cell) exit
      end do
      self%last_edge_cell_per_colour(colour) = cell
    end do

  end subroutine init_last_cell_per_colour

  !> @brief  Gets the index of the last cell in the deepest halo
  !> @details Returns the index of the last cell in a particular depth
  !>          of halo in a 2d slice on the local partition
  !> @return last_halo_cell The index of the last cell in the particular depth
  !>         of halo on the local partition
  !============================================================================
  function get_last_halo_cell_deepest( self ) result ( last_halo_cell )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def)             :: last_halo_cell

    last_halo_cell = self%local_mesh%get_last_halo_cell( self%get_halo_depth() )

  end function get_last_halo_cell_deepest

  !> @brief Check the bounds for depth and colour arguments and aborts
  !>        if out of bounds

  subroutine bounds_check(self, function_name, colour, depth)
    implicit none

    class(mesh_type), intent(in) :: self

    character(len = *), intent(in)  :: function_name

    integer(i_def), intent(in), optional :: colour
    integer(i_def), intent(in), optional :: depth

    if (present(depth)) then
      if( depth > self%get_halo_depth() .or. depth < 0 )then
        write(log_scratch_space,'(A,A,I5,A)')function_name,': depth ', &
           depth,' is out of bounds'
        call log_event(log_scratch_space, LOG_LEVEL_ERROR)
      end if
    end if

    if (present(colour)) then
      if( colour > self%ncolours .or. colour < 1 )then
        write(log_scratch_space,'(A,A,I5,A)')function_name,': colour ', &
           colour,' is out of bounds'
        call log_event(log_scratch_space, LOG_LEVEL_ERROR)
      end if
    end if
  end subroutine bounds_check

  !> @details Get the total number of ghost cells in a slice around
  !>          the local partition
  !> @return  The total number of ghost cells around the local partition
  !============================================================================
  function get_num_cells_ghost( self ) result ( ghost_cells )

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def)               :: ghost_cells

    ghost_cells = self%local_mesh%get_num_cells_ghost()

  end function get_num_cells_ghost


  !> @details Returns the global index of the cell that corresponds
  !>          to the given local index on the local partition
  !> @param[in] cell_lid  The id of a cell in local index space
  !> @return              The id of a cell in global index space
  !============================================================================
  function get_gid_from_lid( self, cell_lid ) result ( cell_gid )

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: cell_lid  ! local index
    integer(i_def)               :: cell_gid  ! global index

    cell_gid = self%local_mesh%get_gid_from_lid(cell_lid)

  end function get_gid_from_lid


  !> @details Returns count of colours used in colouring mesh.
  !> @return          Number of colours used to colour this mesh.
  !============================================================================
  function get_ncolours(self) result(ncolours)
    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: ncolours

    ncolours = self%ncolours

  end function get_ncolours

  !============================================================================
  !> @brief Get the colour map
  !> @param[in] self  The mesh_type instance.
  !> @param[out] colour_map    Indices of cells in each colour.
  !============================================================================
  function get_colour_map(self) result (colour_map)
    implicit none
    class(mesh_type), intent(in), target      :: self
    integer(i_def), pointer                   :: colour_map(:,:)

    colour_map => self%cells_in_colour

  end function get_colour_map

  !============================================================================
  !> @brief Populates args with colouring info.
  !> @param[in] self  The mesh_type instance.
  !> @param[out] ncolours  Number of colours used to colour this mesh.
  !> @param[out] ncells_per_colour  Count of cells in each colour.
  !> @param[out] colour_map         Indices of cells in each colour.
  !============================================================================
  subroutine get_colours(self, ncolours, ncells_per_colour, colour_map)
    implicit none
    class(mesh_type), intent(in), target      :: self
    integer(i_def), intent(out)               :: ncolours
    integer(i_def), pointer, intent(out)  :: ncells_per_colour(:)
    integer(i_def), pointer, intent(out)  :: colour_map(:,:)

    ncolours = self%ncolours
    ncells_per_colour => self%ncells_per_colour
    colour_map => self%cells_in_colour

  end subroutine get_colours

  !============================================================================
  !> @brief  Returns state of colouring: has colouring yet been applied to
  !>         this mesh?
  !>
  !> @return  Logical true is mesh coloured, false if not.
  !============================================================================
  function is_coloured(self) result(cstat)
    implicit none
    class(mesh_type), intent(in) :: self
    logical(l_def)               :: cstat

    if(self%ncolours <= 0) then
      cstat = .false.
    else
      cstat = .true.
    end if

  end function is_coloured

  !> @details Returns count of colours used in mesh tiles.
  !> @return          Number of colours used in mesh tiles
  !============================================================================
  function get_ntilecolours(self) result(ntilecolours)
    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: ntilecolours

    ntilecolours = self%ntilecolours

  end function get_ntilecolours

  !============================================================================
  !> @brief Get the colour tiling map
  !> @param[out] coloured_tiling_map    Indices of cells in each coloured tile.
  !============================================================================
  function get_coloured_tiling_map(self) result (coloured_tiling_map)
    implicit none
    class(mesh_type), intent(in), target :: self
    integer(i_def), pointer              :: coloured_tiling_map(:,:,:)

    coloured_tiling_map => self%cells_in_coloured_tile

  end function get_coloured_tiling_map

  !============================================================================
  !> @brief Populates args with coloured tiling info.
  !> @param[out] ncolours  Number of colours used for mesh tiles.
  !> @param[out] ntiles_per_colour Count of tiles in each colour.
  !> @param[out] ncells_per_coloured_tile Count of cells in each coloured tile.
  !> @param[out] coloured_tiling_map      Cells in each coloured tile.
  !============================================================================
  subroutine get_tiling(self, ntilecolours, ntiles_per_colour, &
                        ncells_per_coloured_tile, coloured_tiling_map)
    implicit none
    class(mesh_type), intent(in), target :: self
    integer(i_def), intent(out)          :: ntilecolours
    integer(i_def), pointer, intent(out) :: ntiles_per_colour(:)
    integer(i_def), pointer, intent(out) :: ncells_per_coloured_tile(:,:)
    integer(i_def), pointer, intent(out) :: coloured_tiling_map(:,:,:)

    ntilecolours = self%ntilecolours
    ntiles_per_colour => self%ntiles_per_colour
    ncells_per_coloured_tile => self%ncells_per_coloured_tile
    coloured_tiling_map => self%cells_in_coloured_tile

  end subroutine get_tiling

  !============================================================================
  !> @brief  Add a mesh map to this objects mesh map collection. The
  !>         map will be from this mesh to the specified
  !>         target mesh.
  !>
  !> @param[in] target_mesh The target mesh to create a map to.
  !============================================================================
  subroutine add_mesh_map(self, target_mesh)

    implicit none

    class(mesh_type), intent(inout)       :: self
    type(mesh_type),  intent(in), pointer :: target_mesh


    type(local_mesh_type), pointer      :: source_local_mesh  => null()
    type(local_mesh_type), pointer      :: target_local_mesh  => null()
    type(local_mesh_map_type), pointer  :: source_mesh_map    => null()

    integer(i_def) :: source_mesh_id
    integer(i_def) :: target_mesh_id
    integer(i_def) :: target_local_mesh_id
    integer(i_def) :: mesh_map_id
    integer(i_def) :: ntarget_cells_per_source_cell_x
    integer(i_def) :: ntarget_cells_per_source_cell_y
    integer(i_def) :: nsource_cells
    logical(l_def) :: mesh_map_exists
    integer(i_def), allocatable :: local_map(:,:,:)

    target_mesh_id = target_mesh%get_id()
    source_mesh_id = self%get_id()
    if (source_mesh_id == target_mesh_id) then
      write(log_scratch_space, '(A)') &
           'Nothing to do, no need to map a local mesh to itself.'
      call log_event(log_scratch_space, LOG_LEVEL_TRACE)
      return
    end if

    mesh_map_id = (10000*source_mesh_id) + target_mesh_id

    ! Ask the mesh map collection if it has this mesh.
    mesh_map_exists = self%mesh_maps%query(mesh_map_id)

    if (.not. mesh_map_exists) then
      ! Get the target mesh id
      target_local_mesh => target_mesh%get_local_mesh()
      target_local_mesh_id = target_local_mesh%get_id()
      ! Find the size of the local mesh map and allocate an array to hold it
      source_local_mesh => self%get_local_mesh()
      source_mesh_map => source_local_mesh%get_local_mesh_map( &
                                                     target_local_mesh_id )
      nsource_cells = source_mesh_map%get_nsource_cells()
      ntarget_cells_per_source_cell_x = &
                      source_mesh_map%get_ntarget_cells_per_source_cell_x()
      ntarget_cells_per_source_cell_y = &
                      source_mesh_map%get_ntarget_cells_per_source_cell_y()
      allocate( local_map ( ntarget_cells_per_source_cell_x, &
                            ntarget_cells_per_source_cell_y, &
                            nsource_cells) )
      ! Get the local mapping and add the mesh map to the mesh object
      call source_mesh_map%get_cell_map( local_map )
      call self%mesh_maps%add_mesh_map( self%get_id(),  &
                                        target_mesh_id, &
                                        local_map )
      deallocate( local_map )
    end if
    return

  end subroutine add_mesh_map

  !============================================================================
  !> @brief  Returns a pointer to the mesh_map object that maps this
  !>         mesh_type object (source) to another mesh_type_object (target).
  !>
  !> @param[in] target_mesh   Pointer to target mesh object.
  !> @retval    mesh_map_type<<pointer>>
  !============================================================================
  function get_mesh_map(self, target_mesh) result(mesh_map)

    implicit none

    class(mesh_type),         intent(in) :: self
    type(mesh_type), pointer, intent(in) :: target_mesh

    type(mesh_map_type), pointer :: mesh_map

    integer(i_def) :: source_mesh_id
    integer(i_def) :: target_mesh_id

    nullify(mesh_map)
    source_mesh_id = self%get_id()
    target_mesh_id = target_mesh%get_id()

    if (source_mesh_id == target_mesh_id) then
      write(log_scratch_space, '(A)') 'Identical source and target meshes.'
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    else
      mesh_map => self%mesh_maps%get_mesh_map(source_mesh_id, target_mesh_id)
    end if

    return
  end function get_mesh_map

  !============================================================================
  !> @brief  Returns a logical whether the mesh map exists
  !>
  !> @param[in] target_mesh   Pointer to target mesh object.
  !> @return    Logical true if mesh_map exists, false if not.
  !============================================================================
  function query_mesh_map(self, target_mesh) result(mesh_map_exists)

    implicit none

    class(mesh_type),         intent(in) :: self
    type(mesh_type), pointer, intent(in) :: target_mesh

    logical(l_def) :: mesh_map_exists

    integer(i_def) :: source_mesh_id
    integer(i_def) :: target_mesh_id
    integer(i_def) :: mesh_map_id

    source_mesh_id = self%get_id()
    target_mesh_id = target_mesh%get_id()

    mesh_map_id = (10000*source_mesh_id) + target_mesh_id
    mesh_map_exists = self%mesh_maps%query(mesh_map_id)

  end function query_mesh_map

  !============================================================================
  !> @details Computes the local ID of each face in the adjacent cells.
  !>          Requires number of 2D (horizontal) and 3D (all) faces from the
  !>          reference element.
  !>          Note: The method relies on enumeration of an opposite face being
  !>          nfaces_h/2 apart, hence giving correct results only for a
  !>          reference element with an even number of horizontal faces (e.g. a
  !>          cube or a hexagonal prism).
  !>
  !> @param[out] face_id_in_adjacent_cell Local index of face in adjacent cells
  !> @param[in]  nfaces_h                 Number of horizontal faces per 3D cell
  !>                                      (faces visited when looping in horizontal)
  !> @param[in]  cell_next                Cell IDs of adjacent cells
  !> @param[in]  nfaces                   Number of all faces in a 3D cell
  !> @param[in]  ncells                   Number of cells in partition
  subroutine calc_face_id_in_adjacent_cell( face_id_in_adjacent_cell, &
                                            nfaces_h,                 &
                                            cell_next,                &
                                            nfaces,                   &
                                            ncells )

    implicit none

    integer(i_def), intent(in)  :: nfaces_h, ncells, nfaces
    integer(i_def), intent(in)  :: cell_next(nfaces,ncells)
    integer(i_def), intent(out) :: face_id_in_adjacent_cell(nfaces_h,ncells)

    integer(i_def) :: cell, face, next_cell, next_face

    ! Check that the number of horizontal faces is even
    if ( mod(nfaces_h, 2_i_def) == 0_i_def ) then

      ! If the number of horizontal faces is even, find indices of adjacent cells
      do cell = 1, ncells
        do face = 1, nfaces_h
          if ( cell_next(face,cell) > 0_i_def ) then
            next_cell = cell_next(face,cell)
            do next_face = 1, nfaces_h
              if ( cell_next(next_face,next_cell) == cell ) then
                ! We have found the local id in next_cell that takes us back to
                ! the original cell, so store this value in the array
                face_id_in_adjacent_cell(face,cell) = next_face
              end if
            end do
          else
            ! There are no neighbour cells so put a negative number in the
            ! adjacency array
            face_id_in_adjacent_cell(face,cell) = mod(face+nfaces_h/2_i_def, nfaces_h)
          end if
        end do
      end do

    else

      ! If the number of horizontal faces is odd report an error
      write(log_scratch_space,'(A,I0)') &
         "calc_face_id_in_adjacent_cell: odd number of horizontal faces ", &
          nfaces_h
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)

    end if

  end subroutine calc_face_id_in_adjacent_cell

  !> @details This function returns the local id of faces in
  !> horizontally adjacent faces
  !> @return adjacent_face local id of faces in neighbouring cells
  !============================================================================
  function get_adjacent_face(self) result (adjacent_face)

  implicit none
  class(mesh_type), target, intent(in) :: self
  integer(i_def),   pointer            :: adjacent_face(:,:)

  adjacent_face => self%face_id_in_adjacent_cell(:,:)

  end function get_adjacent_face

  !============================================================================
  !> @brief  Returns mesh tag name.
  !> @return mesh_name  Tag name of mesh that identifies it.
  !>
  function get_mesh_name( self ) result ( mesh_name )

    implicit none

    class(mesh_type), intent(in) :: self

    character(str_def) :: mesh_name

    mesh_name = self%mesh_name

  end function get_mesh_name


  !============================================================================
  !> @brief  Queries if mesh was created from a spherical surface
  !>         geometry
  !> @return answer .true. for a spherical local mesh geometry
  !>
  function is_geometry_spherical( self ) result ( answer )

    implicit none

    class(mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = self%local_mesh%is_geometry_spherical()

  end function is_geometry_spherical


  !============================================================================
  !> @brief  Queries if mesh was created from a planar surface
  !>         geometry
  !> @return answer .true. for a planar local mesh geometry
  !>
  function is_geometry_planar( self ) result ( answer )

    implicit none

    class(mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = self%local_mesh%is_geometry_planar()

  end function is_geometry_planar


  !============================================================================
  !> @brief  Queries if mesh was created from a non-periodic
  !>         global mesh domain where all the boundaries are closed.
  !>
  !> @return answer .true. for a non-periodic global domain.
  !>
  function is_topology_non_periodic( self ) result ( answer )

    implicit none

    class(mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = self%local_mesh%is_topology_periodic()

  end function is_topology_non_periodic


  !============================================================================
  !> @brief  Queries if mesh was created from a channel
  !>         global mesh domain, which contained a single pair
  !>         of periodic boundaries.
  !>
  !> @return answer .true. for a global channel domain.
  !>
  function is_topology_channel( self ) result ( answer )

    implicit none

    class(mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = self%local_mesh%is_topology_channel()

  end function is_topology_channel

  !============================================================================
  !> @brief  Queries if mesh was created from a periodic
  !>         global mesh domain
  !>
  !> @return answer .true. for a periodic global domain.
  !>
  function is_topology_periodic( self ) result ( answer )

    implicit none

    class(mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = self%local_mesh%is_topology_periodic()

  end function is_topology_periodic


  !============================================================================
  !> @brief  Queries if the mesh nodes are specified using Cartesian
  !>         co-ordinates (x,y,z).
  !>
  !> @return answer .true. if nodes are specified in Cartesian co-ordinates
  !>
  function is_coord_sys_xyz( self ) result ( answer )

    implicit none

    class(mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = self%local_mesh%is_coord_sys_xyz()

  end function is_coord_sys_xyz


  !============================================================================
  !> @brief  Queries if the local mesh nodes are specified using Spherical
  !>         co-ordinates (longitude, latitude).
  !>
  !> @return answer .true. if nodes are specified in spherical co-ordinates
  !>
  function is_coord_sys_ll( self ) result ( answer )

    implicit none

    class(mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = self%local_mesh%is_coord_sys_ll()

  end function is_coord_sys_ll


  !-----------------------------------------------------------------------------
  !  Function to clear up objects - called by destructor
  !-----------------------------------------------------------------------------
  !> @details Explicitly deallocates any allocatable arrays in the mesh object
  !>          to avoid memory leaks
  subroutine clear(self)

    implicit none

    class (mesh_type), intent(inout) :: self

    if (allocated(self%cell_next))         deallocate( self%cell_next )
    if (allocated(self%vert_on_cell))      deallocate( self%vert_on_cell )
    if (allocated(self%face_on_cell))      deallocate( self%face_on_cell )
    if (allocated(self%edge_on_cell))      deallocate( self%edge_on_cell )
    if (allocated(self%vertex_coords))     deallocate( self%vertex_coords )
    if (allocated(self%cell_centre_coords))deallocate( self%cell_centre_coords )
    if (allocated(self%vert_cell_owner))   deallocate( self%vert_cell_owner )
    if (allocated(self%edge_cell_owner))   deallocate( self%edge_cell_owner )
    if (allocated(self%edge_ownership))    deallocate( self%edge_ownership )
    if (allocated(self%vertex_ownership))  deallocate( self%vertex_ownership )
    if (allocated(self%eta))               deallocate( self%eta )
    if (allocated(self%dz))                deallocate( self%dz )
    if (allocated(self%ncells_per_colour)) deallocate( self%ncells_per_colour )
    if (allocated(self%cells_in_colour))   deallocate( self%cells_in_colour )
    if (allocated(self%mesh_maps))         deallocate( self%mesh_maps )

    if (allocated(self%ncells_per_colour_subset))   &
                                  deallocate( self%ncells_per_colour_subset )
    if (allocated(self%last_inner_cell_per_colour)) &
                                  deallocate( self%last_inner_cell_per_colour )
    if (allocated(self%last_halo_cell_per_colour))  &
                                  deallocate( self%last_halo_cell_per_colour )
    if (allocated(self%last_edge_cell_per_colour))  &
                                  deallocate( self%last_edge_cell_per_colour )
    if (allocated(self%face_id_in_adjacent_cell))   &
                                  deallocate( self%face_id_in_adjacent_cell )

    if (allocated(self%ntiles_per_colour)) deallocate(self%ntiles_per_colour)
    if (allocated(self%ncells_per_coloured_tile))          &
                              deallocate(self%ncells_per_coloured_tile)
    if (allocated(self%cells_in_coloured_tile))            &
                              deallocate(self%cells_in_coloured_tile)
    if (allocated(self%last_inner_tile_per_colour))   &
                              deallocate(self%last_inner_tile_per_colour)
    if (allocated(self%last_edge_tile_per_colour))   &
                              deallocate(self%last_edge_tile_per_colour)
    if (allocated(self%last_halo_tile_per_colour))         &
                              deallocate(self%last_halo_tile_per_colour)
    if (allocated(self%last_inner_cell_per_coloured_tile)) &
                              deallocate(self%last_inner_cell_per_coloured_tile)
    if (allocated(self%last_halo_cell_per_coloured_tile))  &
                              deallocate(self%last_halo_cell_per_coloured_tile)
    if (allocated(self%last_edge_cell_per_coloured_tile))  &
                              deallocate(self%last_edge_cell_per_coloured_tile)

    return
  end subroutine clear

  !-----------------------------------------------------------------------------
  !> @brief   Subroutine for debugging mesh
  !-----------------------------------------------------------------------------
  !> @details Mesh details are logged for debug checking.
  !>          This must be explicity called on a mesh as
  !>          the output maybe large.
  !>
  subroutine debug(self)

    use reference_element_mod, only : reference_element_type, &
                                      W, S, E, N, B, T,       &
                                      EB, ET, WB, WT,         &
                                      NB, NT, SB, ST,         &
                                      SW, SE, NW, NE,         &
                                      SWB, SEB, NEB, NWB,     &
                                      SWT, SET, NET, NWT

    implicit none

    class (mesh_type), intent(in) :: self

    integer(i_def)     :: inode, icell
    character(str_def) :: spacer, spacer_long

    spacer      = '========================================='
    spacer_long = '========================================='// &
                  '========================================='

    ! Log cell-cell connectivity
    !=======================================================
    call log_event('', LOG_LEVEL_DEBUG)
    call log_event(spacer, LOG_LEVEL_DEBUG)
    call log_event('Mesh cells adjacent to cells', LOG_LEVEL_DEBUG)
    call log_event(spacer, LOG_LEVEL_DEBUG)
    call log_event('Cell ID  | Cell IDs: S E N W B T', &
                    LOG_LEVEL_DEBUG)
    call log_event(spacer_long, LOG_LEVEL_DEBUG)

    do icell=1, self%ncells
      write(log_scratch_space,'(I6,T9,A,6I7)') &
        icell,' | ',             &
        self%cell_next(S,icell), &
        self%cell_next(E,icell), &
        self%cell_next(N,icell), &
        self%cell_next(W,icell), &
        self%cell_next(B,icell), &
        self%cell_next(T,icell)
      call log_event(log_scratch_space, LOG_LEVEL_DEBUG)
    end do

    ! Report the cell-node connectivity
    !=======================================================
    call log_event('', LOG_LEVEL_DEBUG)
    call log_event(spacer, LOG_LEVEL_DEBUG)
    call log_event('Mesh nodes on cells', LOG_LEVEL_DEBUG)
    call log_event(spacer, LOG_LEVEL_DEBUG)
    call log_event('Cell ID | Node IDs: SWB SEB NEB NWB SWT SET NET NWT', &
                    LOG_LEVEL_DEBUG)
    call log_event(spacer_long, LOG_LEVEL_DEBUG)

    do icell=1, self%ncells
      write(log_scratch_space, '(I6,T9,A,8I7)') &
        icell, '|',                   &
        self%vert_on_cell(SWB,icell), &
        self%vert_on_cell(SEB,icell), &
        self%vert_on_cell(NEB,icell), &
        self%vert_on_cell(NWB,icell), &
        self%vert_on_cell(SWT,icell), &
        self%vert_on_cell(SET,icell), &
        self%vert_on_cell(NET,icell), &
        self%vert_on_cell(NWT,icell)
      call log_event(log_scratch_space, LOG_LEVEL_DEBUG)
    end do

    ! Report the cell-face connectivity
    !=======================================================
    call log_event('', LOG_LEVEL_DEBUG)
    call log_event(spacer, LOG_LEVEL_DEBUG)
    call log_event('Mesh faces on cells', LOG_LEVEL_DEBUG)
    call log_event(spacer, LOG_LEVEL_DEBUG)
    call log_event('Cell ID | Face IDs: S E N W B T', LOG_LEVEL_DEBUG)
    call log_event(spacer_long, LOG_LEVEL_DEBUG)
    do icell=1, self%ncells
      write(log_scratch_space,'(I6,T9,A,6I7)') &
        icell, ' | ',               &
        self%face_on_cell(S,icell), &
        self%face_on_cell(E,icell), &
        self%face_on_cell(N,icell), &
        self%face_on_cell(W,icell), &
        self%face_on_cell(B,icell), &
        self%face_on_cell(T,icell)
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    end do

    ! Report the cell-edge connectivity
    !=======================================================
    call log_event('', LOG_LEVEL_DEBUG)
    call log_event(spacer, LOG_LEVEL_DEBUG)
    call log_event('Mesh edges on cells', LOG_LEVEL_DEBUG)
    call log_event(spacer, LOG_LEVEL_DEBUG)
    call log_event('Cell ID | Edge IDs: SB EB NB WB SW SE NE NW ST ET NT WT', &
                   LOG_LEVEL_DEBUG)
    call log_event(spacer_long, LOG_LEVEL_DEBUG)
    do icell=1, self%ncells
      write( log_scratch_space, '(I6,T9,A,12I7)' )                  &
          icell, ' | ',                                             &
          self%edge_on_cell(SB,icell), self%edge_on_cell(EB,icell), &
          self%edge_on_cell(NB,icell), self%edge_on_cell(WB,icell), &
          self%edge_on_cell(SW,icell), self%edge_on_cell(SE,icell), &
          self%edge_on_cell(NE,icell), self%edge_on_cell(NW,icell), &
          self%edge_on_cell(ST,icell), self%edge_on_cell(ET,icell), &
          self%edge_on_cell(NT,icell), self%edge_on_cell(WT,icell)
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    end do

    ! Log node coordinates (cartesian)
    !=======================================================
    call log_event('', LOG_LEVEL_DEBUG)
    call log_event(spacer, LOG_LEVEL_DEBUG)
    call log_event('Mesh node coordinates (m)', LOG_LEVEL_DEBUG)
    call log_event(spacer, LOG_LEVEL_DEBUG)
    call log_event('Node ID |   x y z', LOG_LEVEL_DEBUG)
    call log_event(spacer_long, LOG_LEVEL_DEBUG)

    do inode=1, self%nverts
      write(log_scratch_space, '(I6,T9,A,3ES20.10E3)') &
          inode, '|', self%vertex_coords(:,inode)
      call log_event(log_scratch_space, LOG_LEVEL_DEBUG)
    end do

  end subroutine debug

  !-----------------------------------------------------------------------------
  ! Mesh destructor
  !-----------------------------------------------------------------------------

  subroutine mesh_destructor(self)

    implicit none

    type (mesh_type), intent(inout) :: self

    call self%clear()

    return
  end subroutine mesh_destructor


  !============================================================================
  ! This routine is only available when setting data for unit testing.
  !============================================================================
  !> @brief     Stucture-Constructor (for unit testing)
  !> @param[in] mesh_cfg Sets the type of test mesh returned.
  !>                     [PLANE|PLANE_BI_PERIODIC].
  !>                     PLANE - returns a 5-layer non-biperiodic mesh
  !>                     PLANE_BI_PERIODIC - returns a 3-layer bi-periodic mesh
  !> @param[in] local_mesh_ptr A pointer to a (unit test) local mesh object
  !> @return             A 3D-Mesh object based on a 3x3-cell global mesh
  !>                     with one partition.
  !============================================================================
  function mesh_constructor_unit_test_data( mesh_cfg, local_mesh ) result(self)

    ! Mesh returned is based on a 3x3 partition with the following
    ! cell numbering.
    !
    !    +-----------+
    !    | 7 | 8 | 9 |
    !    |---+---+---|
    !    | 4 | 5 | 6 |
    !    |---+---+---|
    !    | 1 | 2 | 3 |
    !    +---+---+---+
    !

    use extrusion_mod,         only : uniform_extrusion_type, &
                                      PRIME_EXTRUSION,        &
                                      TWOD
    use reference_element_mod, only : reference_cube_type

    implicit none

    integer(i_def),         intent(in)           :: mesh_cfg
    type(local_mesh_type),  intent(in), pointer  :: local_mesh

    type(mesh_type) :: self

    integer(i_def), parameter :: nverts_per_cell    = 8
    integer(i_def), parameter :: nedges_per_cell    = 12
    integer(i_def), parameter :: nverts_per_2d_cell = 4
    integer(i_def), parameter :: nedges_per_2d_cell = 4

    real(r_def)    :: global_domain_extents(2,4)
    logical(l_def) :: ll_coords

    type(uniform_extrusion_type) :: extrusion

    integer(i_def) :: extrusion_profile

    self%local_mesh => local_mesh
    self%nverts_per_cell = 8
    self%nedges_per_cell = 12
    self%nfaces_per_cell = 6
    self%nverts_per_edge = 2
    self%nverts_per_2d_cell = 4
    self%nedges_per_2d_cell = 4
    self%ncolours        = -1  ! Initialise ncolours to error status

    mesh_id_counter = mesh_id_counter+1
    call self%set_id( mesh_id_counter )

    ! The unit test mesh is quadrilateral, not prismatic
    allocate( self%reference_element, source=reference_cube_type() )



    if (mesh_cfg == PLANE) then
      ! The unit-test mesh is similar in configuration
      ! to a LAM. Node co-ords are held in cartesian,(xyz),
      ! though the <domain_type> object considers the axis
      ! to be in llr, i.e. the domain
      ! units would be in [radians, radians, metres]

      self%mesh_name = 'test mesh: planar'
      self%domain_depth       = 10000.0_r_def
      self%domain_base_height = 30000.0_r_def
      ll_coords = .true.

      self%ncells_2d = 9
      self%nverts_2d = 16
      self%nedges_2d = 24

      self%nlayers = 5
      self%ncells  = 45
      self%nverts  = 96
      self%nfaces  = 174
      self%nedges  = 224

    else if (mesh_cfg == PLANE_BI_PERIODIC) then
      ! The unit-test mesh is on a cartesian domain as
      ! a bi-periodic domain is not supported for
      ! a spherical coord system.
      ! Node co-ords are held in cartesian,(xyz),
      ! as is <domain_type> object, i.e.the axis are xyz
      ! with the units as in metres.
      self%mesh_name = 'test mesh: planar bi-periodic'
      self%domain_depth = 6000.0_r_def
      self%domain_base_height = 0.0_r_def
      ll_coords = .false.

      ! 3x3x3 mesh bi-periodic
      self%ncells_2d = 9
      self%nverts_2d = 9
      self%nedges_2d = 18

      self%nlayers = 3
      self%ncells  = 27
      self%nverts  = 36
      self%nfaces  = 108
      self%nedges  = 99

    else if ( mesh_cfg == PLANE_TWOD ) then
      self%mesh_name = 'test mesh: planar twod'
      self%domain_depth = 10000.0_r_def
      self%domain_base_height = 30000.0_r_def
      ll_coords = .true.

      self%ncells_2d = 9
      self%nverts_2d = 16
      self%nedges_2d = 24

      self%nlayers = 1
      self%ncells  = 9
      self%nverts  = 32
      self%nfaces  = 42
      self%nedges  = 64

    else if ( mesh_cfg == PLANE_TWOD_BI_PERIODIC ) then
      self%mesh_name = 'test mesh: planar twod bi-periodic'
      self%domain_depth = 6000.0_r_def
      self%domain_base_height = 0.0_r_def
      ll_coords = .false.

      self%ncells_2d = 9
      self%nverts_2d = 9
      self%nedges_2d = 18

      self%nlayers = 1
      self%ncells  = 9
      self%nverts  = 18
      self%nfaces  = 36
      self%nedges  = 45

    else
      write(log_scratch_space,'(A,I0)')  &
          "mesh_constructor_unit_test_data:bad mesh specifier:", mesh_cfg
      call log_event(log_scratch_space,LOG_LEVEL_ERROR)
    end if

    self%ncells_2d_with_ghost = self%ncells_2d &
                              + self%local_mesh%get_num_cells_ghost()
    self%ncells_with_ghost    = self%ncells_2d_with_ghost * self%nlayers


    allocate( self%cell_next         ( self%nfaces_per_cell, self%ncells) )
    allocate( self%vert_on_cell      ( self%nverts_per_cell, self%ncells) )
    allocate( self%face_on_cell      ( self%nfaces_per_cell, self%ncells_2d) )
    allocate( self%edge_on_cell      ( self%nedges_per_cell, self%ncells_2d) )
    allocate( self%vertex_coords     ( 3, self%nverts) )
    allocate( self%cell_centre_coords( 3, self%ncells) )

    allocate( self%vert_cell_owner   ( nverts_per_2d_cell, self%ncells_2d ) )
    allocate( self%edge_cell_owner   ( nedges_per_2d_cell, self%ncells_2d ) )
    allocate( self%edge_ownership    ( nedges_per_2d_cell, self%ncells_2d ) )
    allocate( self%vertex_ownership  ( nverts_per_2d_cell, self%ncells_2d ) )

    allocate( self%eta               ( 0:self%nlayers ) )
    allocate( self%dz                ( self%nlayers   ) )

    ! Calculate vertical coordinates eta[0,1] and dz in a separate subroutine
    ! for the unit tests.
    ! Hard wires for uniform vertical grid on planar mesh.
    if ( mesh_cfg == PLANE_TWOD .or. &
         mesh_cfg == PLANE_TWOD_BI_PERIODIC ) then
      extrusion_profile = TWOD
    else
      extrusion_profile = PRIME_EXTRUSION
    end if

    extrusion = uniform_extrusion_type( 0.0_r_def,         &
                                        self%domain_depth, &
                                        self%nlayers,      &
                                        extrusion_profile )
    call extrusion%extrude( self%eta )

    self%extrusion_id = extrusion%get_id()

    if (mesh_cfg == PLANE_BI_PERIODIC .or. mesh_cfg == PLANE_TWOD_BI_PERIODIC) then
      ! Periodic
      self%vert_cell_owner (:,:) = reshape( [ &
          9, 8, 5, 6, &  ! Cell 1
          8, 9, 6, 5, &  ! Cell 2
          9, 9, 6, 6, &  ! Cell 3
          6, 5, 8, 9, &  ! Cell 4
          5, 6, 9, 8, &  ! Cell 5
          6, 6, 9, 9, &  ! Cell 6
          9, 8, 8, 9, &  ! Cell 7
          8, 9, 9, 8, &  ! Cell 8
          9, 9, 9, 9  &  ! Cell 9
          ], shape(self%vert_cell_owner) )

      self%edge_cell_owner (:,:) = reshape( [ &
          3, 7, 2, 4, &  ! Cell 1
          2, 8, 3, 5, &  ! Cell 2
          3, 9, 3, 6, &  ! Cell 3
          6, 4, 5, 7, &  ! Cell 4
          5, 5, 6, 8, &  ! Cell 5
          6, 6, 6, 9, &  ! Cell 6
          9, 7, 8, 7, &  ! Cell 7
          8, 8, 9, 8, &  ! Cell 8
          9, 9, 9, 9  &  ! Cell 9
          ], shape(self%edge_cell_owner) )
    else
      ! Non-periodic
      self%vert_cell_owner (:,:) = reshape( [ &
          1, 2, 5, 4, &  ! Cell 1
          2, 3, 6, 5, &  ! Cell 2
          3, 3, 6, 6, &  ! Cell 3
          4, 5, 8, 7, &  ! Cell 4
          5, 6, 9, 8, &  ! Cell 5
          6, 6, 9, 9, &  ! Cell 6
          7, 8, 8, 7, &  ! Cell 7
          8, 9, 9, 8, &  ! Cell 8
          9, 9, 9, 9  &  ! Cell 9
          ], shape(self%vert_cell_owner) )

      ! left, bottom, right, top
      self%edge_cell_owner (:,:) = reshape( [ &
          1, 1, 2, 4, &  ! Cell 1
          2, 2, 3, 5, &  ! Cell 2
          3, 3, 3, 6, &  ! Cell 3
          4, 4, 5, 7, &  ! Cell 4
          5, 5, 6, 8, &  ! Cell 5
          6, 6, 6, 9, &  ! Cell 6
          7, 7, 8, 7, &  ! Cell 7
          8, 8, 9, 8, &  ! Cell 8
          9, 9, 9, 9  &  ! Cell 9
          ], shape(self%edge_cell_owner) )
    end if

    self%edge_ownership   (:,:) = 0
    self%vertex_ownership (:,:) = 0

    if (mesh_cfg == PLANE) then
      !=========================================================
      ! Assign 3D cell local ids on adjacent to given cell
      !
      ! Index ordering follows:
      ! 1) West
      ! 2) South
      ! 3) East
      ! 4) North
      ! 5) Bottom
      ! 6) Top
      !=========================================================
      ! Layer 1
      self%cell_next(:, 1) = [ 0,  0,  2,  4,  0, 10]
      self%cell_next(:, 2) = [ 1,  0,  3,  5,  0, 11]
      self%cell_next(:, 3) = [ 2,  0,  0,  6,  0, 12]
      self%cell_next(:, 4) = [ 0,  1,  5,  7,  0, 13]
      self%cell_next(:, 5) = [ 4,  2,  6,  8,  0, 14]
      self%cell_next(:, 6) = [ 5,  3,  0,  9,  0, 15]
      self%cell_next(:, 7) = [ 0,  4,  8,  0,  0, 16]
      self%cell_next(:, 8) = [ 7,  5,  9,  0,  0, 17]
      self%cell_next(:, 9) = [ 8,  6,  0,  0,  0, 18]

      ! Layer 2
      self%cell_next(:,10) = [ 0,  0, 11, 13,  1, 19]
      self%cell_next(:,11) = [10,  0, 12, 14,  2, 20]
      self%cell_next(:,12) = [11,  0,  0, 15,  3, 21]
      self%cell_next(:,13) = [ 0, 10, 14, 16,  4, 22]
      self%cell_next(:,14) = [13, 11, 15, 17,  5, 23]
      self%cell_next(:,15) = [14, 12,  0, 18,  6, 24]
      self%cell_next(:,16) = [ 0, 13, 17,  0,  7, 25]
      self%cell_next(:,17) = [16, 14, 18,  0,  8, 26]
      self%cell_next(:,18) = [17, 15,  0,  0,  9, 27]

      ! Layer 3
      self%cell_next(:,19) = [ 0,  0, 20, 22, 10, 28]
      self%cell_next(:,20) = [19,  0, 21, 23, 11, 29]
      self%cell_next(:,21) = [20,  0,  0, 24, 12, 30]
      self%cell_next(:,22) = [ 0, 19, 23, 25, 13, 31]
      self%cell_next(:,23) = [22, 20, 24, 26, 14, 32]
      self%cell_next(:,24) = [23, 21,  0, 27, 15, 33]
      self%cell_next(:,25) = [ 0, 22, 26,  0, 16, 34]
      self%cell_next(:,26) = [25, 23, 27,  0, 17, 35]
      self%cell_next(:,27) = [26, 24,  0,  0, 18, 36]

      ! Layer 4
      self%cell_next(:,28) = [ 0,  0, 29, 31, 19, 37]
      self%cell_next(:,29) = [28,  0, 30, 32, 20, 38]
      self%cell_next(:,30) = [29,  0,  0, 33, 21, 39]
      self%cell_next(:,31) = [ 0, 28, 32, 34, 22, 40]
      self%cell_next(:,32) = [31, 29, 33, 35, 23, 41]
      self%cell_next(:,33) = [32, 30,  0, 36, 24, 42]
      self%cell_next(:,34) = [ 0, 31, 35,  0, 25, 43]
      self%cell_next(:,35) = [34, 32, 36,  0, 26, 44]
      self%cell_next(:,36) = [35, 33,  0,  0, 27, 45]

      ! Layer 5
      self%cell_next(:,37) = [ 0,  0, 38, 40, 18,  0]
      self%cell_next(:,38) = [37,  0, 39, 41, 19,  0]
      self%cell_next(:,39) = [38,  0,  0, 42, 20,  0]
      self%cell_next(:,40) = [ 0, 37, 41, 43, 40,  0]
      self%cell_next(:,41) = [40, 38, 42, 44, 32,  0]
      self%cell_next(:,42) = [41, 39,  0, 45, 33,  0]
      self%cell_next(:,43) = [ 0, 40, 44,  0, 34,  0]
      self%cell_next(:,44) = [43, 41, 45,  0, 35,  0]
      self%cell_next(:,45) = [44, 42,  0,  0, 36,  0]

      !=========================================================
      ! Assign vertex local ids on cell corners
      !
      ! Index ordering follows:
      ! 1) South-West Bottom
      ! 2) South-East Bottom
      ! 3) North-East Bottom
      ! 4) North-West Bottom
      ! 5) South-West Top
      ! 6) South-East Top
      ! 7) North-East Top
      ! 8) North-West Top
      !=========================================================
      ! Layer 1
      self%vert_on_cell(:, 1) = [ 1,  2,  3,  4, 17, 18, 19, 20]
      self%vert_on_cell(:, 2) = [ 2,  5,  6,  3, 18, 21, 22, 19]
      self%vert_on_cell(:, 3) = [ 5,  7,  8,  6, 21, 23, 24, 22]
      self%vert_on_cell(:, 4) = [ 4,  3,  9, 10, 20, 19, 25, 26]
      self%vert_on_cell(:, 5) = [ 3,  6, 11,  9, 19, 22, 27, 25]
      self%vert_on_cell(:, 6) = [ 6,  8, 12, 11, 22, 24, 28, 27]
      self%vert_on_cell(:, 7) = [10,  9, 13, 14, 26, 25, 29, 30]
      self%vert_on_cell(:, 8) = [ 9, 11, 15, 13, 25, 27, 31, 29]
      self%vert_on_cell(:, 9) = [11, 12, 16, 15, 27, 28, 32, 31]

      ! Layer 2
      self%vert_on_cell(:,10) = [17, 18, 19, 20, 33, 34, 35, 36]
      self%vert_on_cell(:,11) = [18, 21, 22, 19, 34, 37, 38, 35]
      self%vert_on_cell(:,12) = [21, 23, 24, 22, 37, 39, 40, 38]
      self%vert_on_cell(:,13) = [20, 19, 25, 26, 36, 35, 41, 42]
      self%vert_on_cell(:,14) = [19, 22, 27, 25, 35, 38, 43, 41]
      self%vert_on_cell(:,15) = [22, 24, 28, 27, 38, 40, 44, 43]
      self%vert_on_cell(:,16) = [26, 25, 29, 30, 42, 41, 45, 46]
      self%vert_on_cell(:,17) = [25, 27, 31, 29, 41, 43, 47, 45]
      self%vert_on_cell(:,18) = [27, 28, 32, 31, 43, 44, 48, 47]

      ! Layer 3
      self%vert_on_cell(:,19) = [33, 34, 35, 36, 49, 50, 51, 52]
      self%vert_on_cell(:,20) = [34, 37, 38, 35, 50, 53, 54, 51]
      self%vert_on_cell(:,21) = [37, 39, 40, 38, 53, 55, 56, 54]
      self%vert_on_cell(:,22) = [36, 35, 41, 42, 52, 51, 57, 58]
      self%vert_on_cell(:,23) = [35, 38, 43, 41, 51, 54, 59, 57]
      self%vert_on_cell(:,24) = [38, 40, 44, 43, 54, 56, 60, 59]
      self%vert_on_cell(:,25) = [42, 41, 45, 46, 58, 57, 61, 62]
      self%vert_on_cell(:,26) = [41, 43, 47, 45, 57, 59, 63, 61]
      self%vert_on_cell(:,27) = [43, 44, 48, 47, 59, 60, 64, 63]

      ! Layer 4
      self%vert_on_cell(:,28) = [49, 50, 51, 52, 65, 66, 67, 68]
      self%vert_on_cell(:,29) = [50, 53, 54, 51, 66, 69, 70, 67]
      self%vert_on_cell(:,30) = [53, 55, 56, 54, 69, 71, 72, 70]
      self%vert_on_cell(:,31) = [52, 51, 57, 58, 68, 67, 73, 74]
      self%vert_on_cell(:,32) = [51, 54, 59, 57, 67, 70, 75, 73]
      self%vert_on_cell(:,33) = [54, 56, 60, 59, 70, 71, 76, 75]
      self%vert_on_cell(:,34) = [58, 57, 61, 62, 74, 73, 77, 78]
      self%vert_on_cell(:,35) = [57, 59, 63, 61, 73, 75, 79, 77]
      self%vert_on_cell(:,36) = [59, 60, 64, 63, 75, 76, 80, 79]

      ! Layer 5
      self%vert_on_cell(:,37) = [65, 66, 67, 68, 81, 82, 83, 84]
      self%vert_on_cell(:,38) = [66, 69, 70, 67, 82, 85, 86, 83]
      self%vert_on_cell(:,39) = [69, 71, 72, 70, 85, 87, 88, 86]
      self%vert_on_cell(:,40) = [68, 67, 73, 74, 84, 83, 89, 90]
      self%vert_on_cell(:,41) = [67, 70, 75, 73, 83, 86, 91, 89]
      self%vert_on_cell(:,42) = [70, 71, 76, 75, 86, 87, 82, 91]
      self%vert_on_cell(:,43) = [74, 73, 77, 78, 90, 89, 93, 94]
      self%vert_on_cell(:,44) = [73, 75, 79, 77, 89, 91, 95, 93]
      self%vert_on_cell(:,45) = [75, 76, 80, 79, 91, 92, 96, 95]

      !=========================================================
      ! Assign edge local ids on cell edges
      !
      ! Index ordering follows:
      ! 1)  West  Bottom
      ! 2)  South Bottom
      ! 3)  East  Bottom
      ! 4)  North Bottom
      ! 5)  South-West Middle
      ! 6)  South-East Middle
      ! 7)  North-East Middle
      ! 8)  North-West Middle
      ! 9)  West  Top
      ! 10) South Top
      ! 11) East  Top
      ! 12) North Top
      !=========================================================
      ! Layer 1
      self%edge_on_cell(:,1) = [ 1,  3,  5,  7,  9, 10, 11, 12,  2,  4,  6,  8]
      self%edge_on_cell(:,2) = [ 5, 13, 15, 17, 10, 19, 20, 11,  6, 14, 16, 18]
      self%edge_on_cell(:,3) = [15, 21, 23, 25, 19, 27, 28, 20, 16, 22, 24, 26]
      self%edge_on_cell(:,4) = [29,  7, 31, 33, 12, 11, 35, 36, 30,  8, 32, 34]
      self%edge_on_cell(:,5) = [31, 17, 37, 39, 11, 20, 41, 35, 32, 18, 38, 40]
      self%edge_on_cell(:,6) = [37, 25, 42, 44, 20, 28, 46, 41, 38, 26, 43, 45]
      self%edge_on_cell(:,7) = [47, 33, 49, 51, 36, 35, 53, 54, 48, 34, 50, 52]
      self%edge_on_cell(:,8) = [49, 39, 55, 57, 35, 41, 59, 53, 50, 40, 56, 58]
      self%edge_on_cell(:,9) = [55, 44, 60, 62, 41, 46, 64, 59, 56, 45, 61, 63]

      !=========================================================
      ! Assign face local ids on cell sides
      !
      ! Index ordering follows:
      ! 1)  West
      ! 2)  South
      ! 3)  East
      ! 4)  North
      ! 5)  Bottom
      ! 6)  Top
      !=========================================================
      ! Layer 1
      self%face_on_cell(:,1) = [ 1,  2,  3,  4,  5,  6]
      self%face_on_cell(:,2) = [ 3,  7,  8,  9, 10, 11]
      self%face_on_cell(:,3) = [ 8, 12, 13, 14, 15, 16]
      self%face_on_cell(:,4) = [17,  4, 18, 19, 20, 21]
      self%face_on_cell(:,5) = [18,  9, 22, 23, 24, 25]
      self%face_on_cell(:,6) = [22, 14, 26, 27, 28, 29]
      self%face_on_cell(:,7) = [30, 19, 31, 32, 33, 34]
      self%face_on_cell(:,8) = [31, 23, 35, 36, 37, 38]
      self%face_on_cell(:,9) = [35, 27, 39, 40, 41, 42]

      !=========================================================
      ! Assign [x,y,z] vertex coords in (m), with [0,0,0] at centre
      ! of planet (radius=30000.0).
      ! Level 0
      self%vertex_coords (:, 1) = [  5195.345687_r_def,  11352.037430_r_def, &
                                    -27278.922805_r_def ]
      self%vertex_coords (:, 2) = [ -6745.352861_r_def,  10505.264651_r_def, &
                                    -27278.922805_r_def ]
      self%vertex_coords (:, 3) = [  8757.797452_r_def, -13639.461402_r_def, &
                                    -25244.129544_r_def ]
      self%vertex_coords (:, 4) = [ -6745.352861_r_def, -14738.864893_r_def, &
                                    -25244.129544_r_def ]
      self%vertex_coords (:, 5) = [ -6745.352861_r_def, -10505.264651_r_def, &
                                    -27278.922805_r_def ]
      self%vertex_coords (:, 6) = [  8757.797452_r_def,  13639.461402_r_def, &
                                    -25244.129544_r_def ]
      self%vertex_coords (:, 7) = [  5195.345687_r_def, -11352.037430_r_def, &
                                    -27278.922805_r_def ]
      self%vertex_coords (:, 8) = [ -6745.352861_r_def,  14738.864893_r_def, &
                                    -25244.129544_r_def ]
      self%vertex_coords (:, 9) = [  8757.797452_r_def, -13639.461402_r_def, &
                                     25244.129544_r_def ]
      self%vertex_coords (:,10) = [ -6745.352861_r_def, -14738.864893_r_def, &
                                     25244.129544_r_def ]
      self%vertex_coords (:,11) = [  8757.797452_r_def,  13639.461402_r_def, &
                                     25244.129544_r_def ]
      self%vertex_coords (:,12) = [ -6745.352861_r_def,  14738.864893_r_def, &
                                     25244.129544_r_def ]
      self%vertex_coords (:,13) = [ -6745.352861_r_def,  10505.264651_r_def, &
                                     27278.922805_r_def ]
      self%vertex_coords (:,14) = [  5195.345687_r_def,  11352.037430_r_def, &
                                     27278.922805_r_def ]
      self%vertex_coords (:,15) = [ -6745.352861_r_def, -10505.264651_r_def, &
                                     27278.922805_r_def ]
      self%vertex_coords (:,16) = [  5195.345687_r_def, -11352.037430_r_def, &
                                     27278.922805_r_def ]

      ! Level 1
      self%vertex_coords (:,17) = [  5541.702066_r_def,  12108.839925_r_def, &
                                    -29097.517658_r_def ]
      self%vertex_coords (:,18) = [ -7195.043052_r_def,  11205.615628_r_def, &
                                    -29097.517658_r_def ]
      self%vertex_coords (:,19) = [  9341.650615_r_def, -14548.758829_r_def, &
                                    -26927.071514_r_def ]
      self%vertex_coords (:,20) = [ -7195.043052_r_def, -15721.455886_r_def, &
                                    -26927.071514_r_def ]
      self%vertex_coords (:,21) = [ -7195.043052_r_def, -11205.615628_r_def, &
                                    -29097.517658_r_def ]
      self%vertex_coords (:,22) = [  9341.650615_r_def,  14548.758829_r_def, &
                                    -26927.071514_r_def ]
      self%vertex_coords (:,23) = [  5541.702066_r_def, -12108.839925_r_def, &
                                    -29097.517658_r_def ]
      self%vertex_coords (:,24) = [ -7195.043052_r_def,  15721.455886_r_def, &
                                    -26927.071514_r_def ]
      self%vertex_coords (:,25) = [  9341.650615_r_def, -14548.758829_r_def, &
                                     26927.071514_r_def ]
      self%vertex_coords (:,26) = [ -7195.043052_r_def, -15721.455886_r_def, &
                                     26927.071514_r_def ]
      self%vertex_coords (:,27) = [  9341.650615_r_def,  14548.758829_r_def, &
                                     26927.071514_r_def ]
      self%vertex_coords (:,28) = [ -7195.043052_r_def,  15721.455886_r_def, &
                                     26927.071514_r_def ]
      self%vertex_coords (:,29) = [ -7195.043052_r_def,  11205.615628_r_def, &
                                     29097.517658_r_def ]
      self%vertex_coords (:,30) = [  5541.702066_r_def,  12108.839925_r_def, &
                                     29097.517658_r_def ]
      self%vertex_coords (:,31) = [ -7195.043052_r_def, -11205.615628_r_def, &
                                     29097.517658_r_def ]
      self%vertex_coords (:,32) = [  5541.702066_r_def, -12108.839925_r_def, &
                                     29097.517658_r_def ]

      ! Level 2
      self%vertex_coords (:,33) = [  5888.058445_r_def,  12865.642420_r_def, &
                                    -30916.112512_r_def ]
      self%vertex_coords (:,34) = [ -7644.733242_r_def,  11905.966605_r_def, &
                                    -30916.112512_r_def ]
      self%vertex_coords (:,35) = [  9925.503779_r_def, -15458.056256_r_def, &
                                    -28610.013483_r_def ]
      self%vertex_coords (:,36) = [ -7644.733242_r_def, -16704.046879_r_def, &
                                    -28610.013483_r_def ]
      self%vertex_coords (:,37) = [ -7644.733242_r_def, -11905.966605_r_def, &
                                    -30916.112512_r_def ]
      self%vertex_coords (:,38) = [  9925.503779_r_def,  15458.056256_r_def, &
                                    -28610.013483_r_def ]
      self%vertex_coords (:,39) = [  5888.058445_r_def, -12865.642420_r_def, &
                                    -30916.112512_r_def ]
      self%vertex_coords (:,40) = [ -7644.733242_r_def,  16704.046879_r_def, &
                                    -28610.013483_r_def ]
      self%vertex_coords (:,41) = [  9925.503779_r_def, -15458.056256_r_def, &
                                     28610.013483_r_def ]
      self%vertex_coords (:,42) = [ -7644.733242_r_def, -16704.046879_r_def, &
                                     28610.013483_r_def ]
      self%vertex_coords (:,43) = [  9925.503779_r_def,  15458.056256_r_def, &
                                     28610.013483_r_def ]
      self%vertex_coords (:,44) = [ -7644.733242_r_def,  16704.046879_r_def, &
                                     28610.013483_r_def ]
      self%vertex_coords (:,45) = [ -7644.733242_r_def,  11905.966605_r_def, &
                                     30916.112512_r_def ]
      self%vertex_coords (:,46) = [  5888.058445_r_def,  12865.642420_r_def, &
                                     30916.112512_r_def ]
      self%vertex_coords (:,47) = [ -7644.733242_r_def, -11905.966605_r_def, &
                                     30916.112512_r_def ]
      self%vertex_coords (:,48) = [  5888.058445_r_def, -12865.642420_r_def, &
                                     30916.112512_r_def ]

      ! Level 3
      self%vertex_coords (:,49) = [  6234.414824_r_def,  13622.444916_r_def, &
                                    -32734.707366_r_def ]
      self%vertex_coords (:,50) = [ -8094.423433_r_def,  12606.317581_r_def, &
                                    -32734.707366_r_def ]
      self%vertex_coords (:,51) = [ 10509.356942_r_def, -16367.353683_r_def, &
                                    -30292.955453_r_def ]
      self%vertex_coords (:,52) = [ -8094.423433_r_def, -17686.637872_r_def, &
                                    -30292.955453_r_def ]
      self%vertex_coords (:,53) = [ -8094.423433_r_def, -12606.317581_r_def, &
                                    -32734.707366_r_def ]
      self%vertex_coords (:,54) = [ 10509.356942_r_def,  16367.353683_r_def, &
                                    -30292.955453_r_def ]
      self%vertex_coords (:,55) = [  6234.414824_r_def, -13622.444916_r_def, &
                                    -32734.707366_r_def ]
      self%vertex_coords (:,56) = [ -8094.423433_r_def,  17686.637872_r_def, &
                                    -30292.955453_r_def ]
      self%vertex_coords (:,57) = [ 10509.356942_r_def, -16367.353683_r_def, &
                                     30292.955453_r_def ]
      self%vertex_coords (:,58) = [ -8094.423433_r_def, -17686.637872_r_def, &
                                     30292.955453_r_def ]
      self%vertex_coords (:,59) = [ 10509.356942_r_def,  16367.353683_r_def, &
                                     30292.955453_r_def ]
      self%vertex_coords (:,60) = [ -8094.423433_r_def,  17686.637872_r_def, &
                                     30292.955453_r_def ]
      self%vertex_coords (:,61) = [ -8094.423433_r_def,  12606.317581_r_def, &
                                     32734.707366_r_def ]
      self%vertex_coords (:,62) = [  6234.414824_r_def,  13622.444916_r_def, &
                                     32734.707366_r_def ]
      self%vertex_coords (:,63) = [ -8094.423433_r_def, -12606.317581_r_def, &
                                     32734.707366_r_def ]
      self%vertex_coords (:,64) = [  6234.414824_r_def, -13622.444916_r_def, &
                                     32734.707366_r_def ]

      ! Level 4
      self%vertex_coords (:,65) = [  6580.771204_r_def,  14379.247411_r_def, &
                                    -34553.302219_r_def ]
      self%vertex_coords (:,66) = [ -8544.113624_r_def,  13306.668558_r_def, &
                                    -34553.302219_r_def ]
      self%vertex_coords (:,67) = [ 11093.210106_r_def, -17276.651110_r_def, &
                                    -31975.897423_r_def ]
      self%vertex_coords (:,68) = [ -8544.113624_r_def, -18669.228864_r_def, &
                                    -31975.897423_r_def ]
      self%vertex_coords (:,69) = [ -8544.113624_r_def, -13306.668558_r_def, &
                                    -34553.302219_r_def ]
      self%vertex_coords (:,70) = [ 11093.210106_r_def,  17276.651110_r_def, &
                                    -31975.897423_r_def ]
      self%vertex_coords (:,71) = [  6580.771204_r_def, -14379.247411_r_def, &
                                    -34553.302219_r_def ]
      self%vertex_coords (:,72) = [ -8544.113624_r_def,  18669.228864_r_def, &
                                    -31975.897423_r_def ]
      self%vertex_coords (:,73) = [ 11093.210106_r_def, -17276.651110_r_def, &
                                     31975.897423_r_def ]
      self%vertex_coords (:,74) = [ -8544.113624_r_def, -18669.228864_r_def, &
                                     31975.897423_r_def ]
      self%vertex_coords (:,75) = [ 11093.210106_r_def,  17276.651110_r_def, &
                                     31975.897423_r_def ]
      self%vertex_coords (:,76) = [ -8544.113624_r_def,  18669.228864_r_def, &
                                     31975.897423_r_def ]
      self%vertex_coords (:,77) = [ -8544.113624_r_def,  13306.668558_r_def, &
                                     34553.302219_r_def ]
      self%vertex_coords (:,78) = [  6580.771204_r_def,  14379.247411_r_def, &
                                     34553.302219_r_def ]
      self%vertex_coords (:,79) = [ -8544.113624_r_def, -13306.668558_r_def, &
                                     34553.302219_r_def ]
      self%vertex_coords (:,80) = [  6580.771204_r_def, -14379.247411_r_def, &
                                     34553.302219_r_def ]

      ! Level 5
      self%vertex_coords (:,81) = [  6927.127583_r_def,  15136.049906_r_def, &
                                    -36371.897073_r_def ]
      self%vertex_coords (:,82) = [ -8993.803815_r_def,  14007.019535_r_def, &
                                    -36371.897073_r_def ]
      self%vertex_coords (:,83) = [ 11677.063269_r_def, -18185.948537_r_def, &
                                    -33658.839392_r_def ]
      self%vertex_coords (:,84) = [ -8993.803815_r_def, -19651.819857_r_def, &
                                    -33658.839392_r_def ]
      self%vertex_coords (:,85) = [ -8993.803815_r_def, -14007.019535_r_def, &
                                    -36371.897073_r_def ]
      self%vertex_coords (:,86) = [ 11677.063269_r_def,  18185.948537_r_def, &
                                    -33658.839392_r_def ]
      self%vertex_coords (:,87) = [  6927.127583_r_def, -15136.049906_r_def, &
                                    -36371.897073_r_def ]
      self%vertex_coords (:,88) = [ -8993.803815_r_def,  19651.819857_r_def, &
                                    -33658.839392_r_def ]
      self%vertex_coords (:,89) = [ 11677.063269_r_def, -18185.948536_r_def, &
                                     33658.839392_r_def ]
      self%vertex_coords (:,90) = [ -8993.803815_r_def, -19651.819857_r_def, &
                                     33658.839392_r_def ]
      self%vertex_coords (:,91) = [ 11677.063269_r_def,  18185.948536_r_def, &
                                     33658.839392_r_def ]
      self%vertex_coords (:,92) = [ -8993.803815_r_def,  19651.819857_r_def, &
                                     33658.839392_r_def ]
      self%vertex_coords (:,93) = [ -8993.803815_r_def,  14007.019535_r_def, &
                                     36371.897073_r_def ]
      self%vertex_coords (:,94) = [  6927.127583_r_def,  15136.049906_r_def, &
                                     36371.897073_r_def ]
      self%vertex_coords (:,95) = [ -8993.803815_r_def, -14007.019535_r_def, &
                                     36371.897073_r_def ]
      self%vertex_coords (:,96) = [  6927.127582_r_def, -15136.049906_r_def, &
                                     36371.897073_r_def ]

      !=========================================================
      ! Assign [x,y,z] cell centre coords in (m), with [0,0,0] at centre
      ! of planet (radius=30000.0).
      ! layer 1
      self%cell_centre_coords (:, 1) = [   155.116303_r_def, &
                                      -2187.360125_r_def, -30922.344585_r_def ]
      self%cell_centre_coords (:, 2) = [  2192.853252_r_def, &
                                     -30922.344585_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:, 3) = [   155.116303_r_def, &
                                      -2187.360125_r_def,  30922.344585_r_def ]
      self%cell_centre_coords (:, 4) = [  2192.853252_r_def, &
                                          0.000000_r_def, -30922.344585_r_def ]
      self%cell_centre_coords (:, 5) = [ 31000.000000_r_def, &
                                          0.000000_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:, 6) = [  2192.853252_r_def, &
                                          0.000000_r_def,  30922.344585_r_def ]
      self%cell_centre_coords (:, 7) = [   155.116303_r_def, &
                                       2187.360125_r_def, -30922.344585_r_def ]
      self%cell_centre_coords (:, 8) = [  2192.853252_r_def, &
                                      30922.344585_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:, 9) = [   155.116303_r_def, &
                                       2187.360125_r_def,  30922.344585_r_def ]
      ! Layer 2
      self%cell_centre_coords (:,10) = [   165.123806_r_def, &
                                      -2328.480133_r_def, -32917.334558_r_def ]
      self%cell_centre_coords (:,11) = [  2334.327655_r_def, &
                                     -32917.334558_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:,12) = [   165.123806_r_def, &
                                      -2328.480133_r_def,  32917.334558_r_def ]
      self%cell_centre_coords (:,13) = [  2334.327655_r_def, &
                                          0.000000_r_def, -32917.334558_r_def ]
      self%cell_centre_coords (:,14) = [ 33000.000000_r_def, &
                                          0.000000_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:,15) = [  2334.327655_r_def, &
                                          0.000000_r_def,  32917.334558_r_def ]
      self%cell_centre_coords (:,16) = [   165.123806_r_def, &
                                       2328.480133_r_def, -32917.334558_r_def ]
      self%cell_centre_coords (:,17) = [  2334.327655_r_def, &
                                      32917.334558_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:,18) = [   165.123806_r_def, &
                                       2328.480133_r_def,  32917.334558_r_def ]
      ! Layer 3
      self%cell_centre_coords (:,19) = [   175.131309_r_def, &
                                      -2469.600141_r_def, -34912.324531_r_def ]
      self%cell_centre_coords (:,20) = [  2475.802058_r_def, &
                                     -34912.324531_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:,21) = [   175.131309_r_def, &
                                      -2469.600141_r_def,  34912.324531_r_def ]
      self%cell_centre_coords (:,22) = [  2475.802058_r_def, &
                                          0.000000_r_def, -34912.324531_r_def ]
      self%cell_centre_coords (:,23) = [ 35000.000000_r_def, &
                                          0.000000_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:,24) = [  2475.802058_r_def, &
                                          0.000000_r_def,  34912.324531_r_def ]
      self%cell_centre_coords (:,25) = [   175.131309_r_def, &
                                       2469.600141_r_def, -34912.324531_r_def ]
      self%cell_centre_coords (:,26) = [  2475.802058_r_def, &
                                      34912.324531_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:,27) = [   175.131309_r_def, &
                                       2469.600141_r_def,  34912.324531_r_def ]
      ! Layer 4
      self%cell_centre_coords (:,28) = [   185.138813_r_def, &
                                      -2610.720149_r_def, -36907.314504_r_def ]
      self%cell_centre_coords (:,29) = [  2617.276462_r_def, &
                                     -36907.314504_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:,30) = [   185.138813_r_def, &
                                      -2610.720149_r_def,  36907.314504_r_def ]
      self%cell_centre_coords (:,31) = [  2617.276462_r_def, &
                                          0.000000_r_def, -36907.314504_r_def ]
      self%cell_centre_coords (:,32) = [ 37000.000000_r_def, &
                                          0.000000_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:,33) = [  2617.276462_r_def, &
                                          0.000000_r_def,  36907.314504_r_def ]
      self%cell_centre_coords (:,34) = [   185.138813_r_def, &
                                       2610.720149_r_def, -36907.314504_r_def ]
      self%cell_centre_coords (:,35) = [  2617.276462_r_def, &
                                      36907.314504_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:,36) = [   185.138813_r_def, &
                                       2610.720149_r_def,  36907.314504_r_def ]
      ! Layer 5
      self%cell_centre_coords (:,37) = [   195.146316_r_def, &
                                      -2751.840157_r_def, -38902.304478_r_def ]
      self%cell_centre_coords (:,38) = [  2758.750865_r_def, &
                                     -38902.304478_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:,39) = [   195.146316_r_def, &
                                      -2751.840157_r_def,  38902.304478_r_def ]
      self%cell_centre_coords (:,40) = [  2758.750865_r_def, &
                                          0.000000_r_def, -38902.304478_r_def ]
      self%cell_centre_coords (:,41) = [ 39000.000000_r_def, &
                                          0.000000_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:,42) = [  2758.750865_r_def, &
                                          0.000000_r_def,  38902.304478_r_def ]
      self%cell_centre_coords (:,43) = [   195.146316_r_def, &
                                       2751.840157_r_def, -38902.304478_r_def ]
      self%cell_centre_coords (:,44) = [  2758.750865_r_def, &
                                      38902.304478_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:,45) = [   195.146316_r_def, &
                                       2751.840157_r_def,  38902.304478_r_def ]

      ! Domain limits
      global_domain_extents(:,1) = [ -1.0_r_def, -0.5_r_def ]*PI
      global_domain_extents(:,2) = [  1.0_r_def, -0.5_r_def ]*PI
      global_domain_extents(:,3) = [  1.0_r_def,  0.5_r_def ]*PI
      global_domain_extents(:,4) = [ -1.0_r_def,  0.5_r_def ]*PI

      self%domain = domain_type( global_domain_extents,   &
                                 self%domain_base_height, &
                                 self%domain_depth,       &
                                 ll_coords )

    else if (mesh_cfg == PLANE_BI_PERIODIC) then
      !=========================================================
      ! Assign 3D cell local ids on adjacent to given cell
      !
      ! Index ordering follows:
      ! 1) West
      ! 2) South
      ! 3) East
      ! 4) North
      ! 5) Bottom
      ! 6) Top
      !=========================================================
      ! Layer 1
      self%cell_next(:, 1) = [ 3,  7,  2,  4,  0, 10]
      self%cell_next(:, 2) = [ 1,  8,  3,  5,  0, 11]
      self%cell_next(:, 3) = [ 2,  9,  1,  6,  0, 12]
      self%cell_next(:, 4) = [ 6,  1,  5,  7,  0, 13]
      self%cell_next(:, 5) = [ 4,  2,  6,  8,  0, 14]
      self%cell_next(:, 6) = [ 5,  3,  4,  9,  0, 15]
      self%cell_next(:, 7) = [ 9,  4,  8,  1,  0, 16]
      self%cell_next(:, 8) = [ 7,  5,  9,  2,  0, 17]
      self%cell_next(:, 9) = [ 8,  6,  7,  3,  0, 18]

      ! Layer 2
      self%cell_next(:,10) = [12, 16, 11, 13,  1, 19]
      self%cell_next(:,11) = [10, 17, 12, 14,  2, 20]
      self%cell_next(:,12) = [11, 18, 10, 15,  3, 21]
      self%cell_next(:,13) = [15, 10, 14, 16,  4, 22]
      self%cell_next(:,14) = [13, 11, 15, 17,  5, 23]
      self%cell_next(:,15) = [14, 12, 13, 18,  6, 24]
      self%cell_next(:,16) = [18, 13, 17, 10,  7, 25]
      self%cell_next(:,17) = [16, 14, 18, 11,  8, 26]
      self%cell_next(:,18) = [17, 15, 16, 12,  9, 27]

      ! Layer 3
      self%cell_next(:,19) = [21, 25, 20, 22, 10, 28]
      self%cell_next(:,20) = [19, 26, 21, 23, 11, 29]
      self%cell_next(:,21) = [20, 27, 19, 24, 12, 30]
      self%cell_next(:,22) = [24, 19, 23, 25, 13, 31]
      self%cell_next(:,23) = [22, 20, 24, 26, 14, 32]
      self%cell_next(:,24) = [23, 21, 22, 27, 15, 33]
      self%cell_next(:,25) = [27, 22, 26, 19, 16, 34]
      self%cell_next(:,26) = [25, 23, 27, 20, 17, 35]
      self%cell_next(:,27) = [26, 24, 25, 21, 18, 36]



      !=========================================================
      ! Assign vertex local ids on cell corners
      !
      ! Index ordering follows:
      ! 1) South-West Bottom
      ! 2) South-East Bottom
      ! 3) North-East Bottom
      ! 4) North-West Bottom
      ! 5) South-West Top
      ! 6) South-East Top
      ! 7) North-East Top
      ! 8) North-West Top
      !=========================================================
      ! Layer 1
      self%vert_on_cell(:, 1) = [ 1,  2,  3,  4, 10, 11, 12, 13]
      self%vert_on_cell(:, 2) = [ 2,  5,  6,  3, 11, 14, 15, 12]
      self%vert_on_cell(:, 3) = [ 5,  1,  4,  6, 14, 10, 13, 15]
      self%vert_on_cell(:, 4) = [ 4,  3,  7,  8, 13, 12, 16, 17]
      self%vert_on_cell(:, 5) = [ 3,  6,  9,  7, 12, 15, 18, 16]
      self%vert_on_cell(:, 6) = [ 6,  4,  8,  9, 15, 13, 17, 18]
      self%vert_on_cell(:, 7) = [ 8,  7,  2,  1, 17, 16, 11, 10]
      self%vert_on_cell(:, 8) = [ 7,  9,  5,  2, 16, 18, 14, 11]
      self%vert_on_cell(:, 9) = [ 9,  8,  1,  5, 18, 17, 10, 14]


      ! Layer 2
      self%vert_on_cell(:,10) = [10, 11, 12, 13, 19, 20, 21, 22]
      self%vert_on_cell(:,11) = [11, 14, 15, 12, 20, 23, 24, 21]
      self%vert_on_cell(:,12) = [14, 10, 13, 15, 23, 19, 22, 24]
      self%vert_on_cell(:,13) = [13, 12, 16, 17, 22, 21, 25, 26]
      self%vert_on_cell(:,14) = [12, 15, 18, 16, 21, 24, 27, 25]
      self%vert_on_cell(:,15) = [15, 13, 17, 18, 24, 22, 26, 27]
      self%vert_on_cell(:,16) = [17, 16, 11, 10, 26, 25, 20, 19]
      self%vert_on_cell(:,17) = [16, 18, 14, 11, 25, 27, 23, 20]
      self%vert_on_cell(:,18) = [18, 17, 10, 14, 27, 26, 19, 23]

      ! Layer 3
      self%vert_on_cell(:,19) = [19, 20, 21, 22, 28, 29, 30, 31]
      self%vert_on_cell(:,20) = [20, 23, 24, 21, 29, 32, 33, 30]
      self%vert_on_cell(:,21) = [23, 19, 22, 24, 32, 28, 31, 33]
      self%vert_on_cell(:,22) = [22, 21, 25, 26, 31, 30, 34, 35]
      self%vert_on_cell(:,23) = [21, 24, 27, 25, 30, 33, 36, 34]
      self%vert_on_cell(:,24) = [24, 22, 26, 27, 33, 31, 35, 36]
      self%vert_on_cell(:,25) = [26, 25, 20, 19, 35, 34, 29, 28]
      self%vert_on_cell(:,26) = [25, 27, 23, 20, 34, 36, 32, 29]
      self%vert_on_cell(:,27) = [27, 26, 19, 23, 36, 35, 28, 32]

      !=========================================================
      ! Assign edge local ids on cell edges
      !
      ! Index ordering follows:
      ! 1)  West  Bottom
      ! 2)  South Bottom
      ! 3)  East  Bottom
      ! 4)  North Bottom
      ! 5)  South-West Middle
      ! 6)  South-East Middle
      ! 7)  North-East Middle
      ! 8)  North-West Middle
      ! 9)  West  Top
      ! 10) South Top
      ! 11) East  Top
      ! 12) North Top
      !=========================================================
      ! Layer 1
      self%edge_on_cell(:,1) = [ 1,  3,  5,  7,  9, 10, 11, 12,  2,  4,  6,  8]
      self%edge_on_cell(:,2) = [ 5, 13, 15, 17, 10, 19, 20, 11,  6, 14, 16, 18]
      self%edge_on_cell(:,3) = [15, 21,  1, 23, 19,  9, 12, 20, 16, 22,  2, 24]
      self%edge_on_cell(:,4) = [25,  7, 27, 29, 12, 11, 31, 32, 26,  8, 28, 30]
      self%edge_on_cell(:,5) = [27, 17, 33, 35, 11, 20, 37, 31, 28, 18, 34, 36]
      self%edge_on_cell(:,6) = [33, 23, 25, 38, 20, 12, 32, 37, 34, 24, 26, 39]
      self%edge_on_cell(:,7) = [40, 29, 42,  3, 32, 31, 10,  9, 41, 30, 43,  4]
      self%edge_on_cell(:,8) = [42, 35, 44, 13, 31, 37, 19, 10, 43, 36, 45, 14]
      self%edge_on_cell(:,9) = [44, 38, 40, 21, 37, 32,  9, 19, 45, 39, 41, 22]
      !=========================================================
      ! Assign face local ids on cell sides
      !
      ! Index ordering follows:
      ! 1)  West
      ! 2)  South
      ! 3)  East
      ! 4)  North
      ! 5)  Bottom
      ! 6)  Top
      !=========================================================
      self%face_on_cell(:,1) = [ 1,  2,  3,  4,  5,  6]
      self%face_on_cell(:,2) = [ 3,  7,  8,  9, 10, 11]
      self%face_on_cell(:,3) = [ 8, 12,  1, 13, 14, 15]
      self%face_on_cell(:,4) = [16,  4, 17, 18, 19, 20]
      self%face_on_cell(:,5) = [17,  9, 21, 22, 23, 24]
      self%face_on_cell(:,6) = [21, 13, 16, 25, 26, 27]
      self%face_on_cell(:,7) = [28, 18, 29,  2, 30, 31]
      self%face_on_cell(:,8) = [29, 22, 32,  7, 33, 34]
      self%face_on_cell(:,9) = [32, 25, 28, 12, 35, 36]

      ! Domain limits
      global_domain_extents(:,1) = [ -15.0_r_def, -15.0_r_def ]
      global_domain_extents(:,2) = [  15.0_r_def, -15.0_r_def ]
      global_domain_extents(:,3) = [  15.0_r_def,  15.0_r_def ]
      global_domain_extents(:,4) = [ -15.0_r_def,  15.0_r_def ]

      self%domain = domain_type( global_domain_extents,   &
                                 self%domain_base_height, &
                                 self%domain_depth,       &
                                 ll_coords )

    else if ( mesh_cfg == PLANE_TWOD ) then
      !=========================================================
      ! Assign 3D cell local ids on adjacent to given cell
      !
      ! Index ordering follows:
      ! 1) West
      ! 2) South
      ! 3) East
      ! 4) North
      ! 5) Bottom
      ! 6) Top
      !=========================================================
      ! Layer 1
      self%cell_next(:, 1) = [ 0,  0,  2,  4,  0,  0]
      self%cell_next(:, 2) = [ 1,  0,  3,  5,  0,  0]
      self%cell_next(:, 3) = [ 2,  0,  0,  6,  0,  0]
      self%cell_next(:, 4) = [ 0,  1,  5,  7,  0,  0]
      self%cell_next(:, 5) = [ 4,  2,  6,  8,  0,  0]
      self%cell_next(:, 6) = [ 5,  3,  0,  9,  0,  0]
      self%cell_next(:, 7) = [ 0,  4,  8,  0,  0,  0]
      self%cell_next(:, 8) = [ 7,  5,  9,  0,  0,  0]
      self%cell_next(:, 9) = [ 8,  6,  0,  0,  0,  0]

      !=========================================================
      ! Assign vertex local ids on cell corners
      !
      ! Index ordering follows:
      ! 1) South-West Bottom
      ! 2) South-East Bottom
      ! 3) North-East Bottom
      ! 4) North-West Bottom
      ! 5) South-West Top
      ! 6) South-East Top
      ! 7) North-East Top
      ! 8) North-West Top
      !=========================================================
      ! Layer 1
      self%vert_on_cell(:, 1) = [ 1,  2,  3,  4, 17, 18, 19, 20]
      self%vert_on_cell(:, 2) = [ 2,  5,  6,  3, 18, 21, 22, 19]
      self%vert_on_cell(:, 3) = [ 5,  7,  8,  6, 21, 23, 24, 22]
      self%vert_on_cell(:, 4) = [ 4,  3,  9, 10, 20, 19, 25, 26]
      self%vert_on_cell(:, 5) = [ 3,  6, 11,  9, 19, 22, 27, 25]
      self%vert_on_cell(:, 6) = [ 6,  8, 12, 11, 22, 24, 28, 27]
      self%vert_on_cell(:, 7) = [10,  9, 13, 14, 26, 25, 29, 30]
      self%vert_on_cell(:, 8) = [ 9, 11, 15, 13, 25, 27, 31, 29]
      self%vert_on_cell(:, 9) = [11, 12, 16, 15, 27, 28, 32, 31]

      !=========================================================
      ! Assign edge local ids on cell edges
      !
      ! Index ordering follows:
      ! 1)  West  Bottom
      ! 2)  South Bottom
      ! 3)  East  Bottom
      ! 4)  North Bottom
      ! 5)  South-West Middle
      ! 6)  South-East Middle
      ! 7)  North-East Middle
      ! 8)  North-West Middle
      ! 9)  West  Top
      ! 10) South Top
      ! 11) East  Top
      ! 12) North Top
      !=========================================================
      ! Layer 1
      self%edge_on_cell(:,1) = [ 1,  3,  5,  7,  9, 10, 11, 12,  2,  4,  6,  8]
      self%edge_on_cell(:,2) = [ 5, 13, 15, 17, 10, 19, 20, 11,  6, 14, 16, 18]
      self%edge_on_cell(:,3) = [15, 21, 23, 25, 19, 27, 28, 20, 16, 22, 24, 26]
      self%edge_on_cell(:,4) = [29,  7, 31, 33, 12, 11, 35, 36, 30,  8, 32, 34]
      self%edge_on_cell(:,5) = [31, 17, 37, 39, 11, 20, 41, 35, 32, 18, 38, 40]
      self%edge_on_cell(:,6) = [37, 25, 42, 44, 20, 28, 46, 41, 38, 26, 43, 45]
      self%edge_on_cell(:,7) = [47, 33, 49, 51, 36, 35, 53, 54, 48, 34, 50, 52]
      self%edge_on_cell(:,8) = [49, 39, 55, 57, 35, 41, 59, 53, 50, 40, 56, 58]
      self%edge_on_cell(:,9) = [55, 44, 60, 62, 41, 46, 64, 59, 56, 45, 61, 63]

      !=========================================================
      ! Assign face local ids on cell sides
      !
      ! Index ordering follows:
      ! 1)  West
      ! 2)  South
      ! 3)  East
      ! 4)  North
      ! 5)  Bottom
      ! 6)  Top
      !=========================================================
      ! Layer 1
      self%face_on_cell(:,1) = [ 1,  2,  3,  4,  5,  6]
      self%face_on_cell(:,2) = [ 3,  7,  8,  9, 10, 11]
      self%face_on_cell(:,3) = [ 8, 12, 13, 14, 15, 16]
      self%face_on_cell(:,4) = [17,  4, 18, 19, 20, 21]
      self%face_on_cell(:,5) = [18,  9, 22, 23, 24, 25]
      self%face_on_cell(:,6) = [22, 14, 26, 27, 28, 29]
      self%face_on_cell(:,7) = [30, 19, 31, 32, 33, 34]
      self%face_on_cell(:,8) = [31, 23, 35, 36, 37, 38]
      self%face_on_cell(:,9) = [35, 27, 39, 40, 41, 42]

      !=========================================================
      ! Assign [x,y,z] vertex coords in (m), with [0,0,0] at centre
      ! of planet (radius=30000.0).
      ! Level 0
      self%vertex_coords (:, 1) = [  5195.345687_r_def,  11352.037430_r_def, &
                                    -27278.922805_r_def ]
      self%vertex_coords (:, 2) = [ -6745.352861_r_def,  10505.264651_r_def, &
                                    -27278.922805_r_def ]
      self%vertex_coords (:, 3) = [  8757.797452_r_def, -13639.461402_r_def, &
                                    -25244.129544_r_def ]
      self%vertex_coords (:, 4) = [ -6745.352861_r_def, -14738.864893_r_def, &
                                    -25244.129544_r_def ]
      self%vertex_coords (:, 5) = [ -6745.352861_r_def, -10505.264651_r_def, &
                                    -27278.922805_r_def ]
      self%vertex_coords (:, 6) = [  8757.797452_r_def,  13639.461402_r_def, &
                                    -25244.129544_r_def ]
      self%vertex_coords (:, 7) = [  5195.345687_r_def, -11352.037430_r_def, &
                                    -27278.922805_r_def ]
      self%vertex_coords (:, 8) = [ -6745.352861_r_def,  14738.864893_r_def, &
                                    -25244.129544_r_def ]
      self%vertex_coords (:, 9) = [  8757.797452_r_def, -13639.461402_r_def, &
                                      25244.129544_r_def ]
      self%vertex_coords (:,10) = [ -6745.352861_r_def, -14738.864893_r_def, &
                                      25244.129544_r_def ]
      self%vertex_coords (:,11) = [  8757.797452_r_def,  13639.461402_r_def, &
                                      25244.129544_r_def ]
      self%vertex_coords (:,12) = [ -6745.352861_r_def,  14738.864893_r_def, &
                                      25244.129544_r_def ]
      self%vertex_coords (:,13) = [ -6745.352861_r_def,  10505.264651_r_def, &
                                      27278.922805_r_def ]
      self%vertex_coords (:,14) = [  5195.345687_r_def,  11352.037430_r_def, &
                                      27278.922805_r_def ]
      self%vertex_coords (:,15) = [ -6745.352861_r_def, -10505.264651_r_def, &
                                      27278.922805_r_def ]
      self%vertex_coords (:,16) = [  5195.345687_r_def, -11352.037430_r_def, &
                                      27278.922805_r_def ]

      ! Level 1
      self%vertex_coords (:,17) = [  5541.702066_r_def,  12108.839925_r_def, &
                                    -29097.517658_r_def ]
      self%vertex_coords (:,18) = [ -7195.043052_r_def,  11205.615628_r_def, &
                                    -29097.517658_r_def ]
      self%vertex_coords (:,19) = [  9341.650615_r_def, -14548.758829_r_def, &
                                    -26927.071514_r_def ]
      self%vertex_coords (:,20) = [ -7195.043052_r_def, -15721.455886_r_def, &
                                    -26927.071514_r_def ]
      self%vertex_coords (:,21) = [ -7195.043052_r_def, -11205.615628_r_def, &
                                    -29097.517658_r_def ]
      self%vertex_coords (:,22) = [  9341.650615_r_def,  14548.758829_r_def, &
                                    -26927.071514_r_def ]
      self%vertex_coords (:,23) = [  5541.702066_r_def, -12108.839925_r_def, &
                                    -29097.517658_r_def ]
      self%vertex_coords (:,24) = [ -7195.043052_r_def,  15721.455886_r_def, &
                                    -26927.071514_r_def ]
      self%vertex_coords (:,25) = [  9341.650615_r_def, -14548.758829_r_def, &
                                      26927.071514_r_def ]
      self%vertex_coords (:,26) = [ -7195.043052_r_def, -15721.455886_r_def, &
                                      26927.071514_r_def ]
      self%vertex_coords (:,27) = [  9341.650615_r_def,  14548.758829_r_def, &
                                      26927.071514_r_def ]
      self%vertex_coords (:,28) = [ -7195.043052_r_def,  15721.455886_r_def, &
                                      26927.071514_r_def ]
      self%vertex_coords (:,29) = [ -7195.043052_r_def,  11205.615628_r_def, &
                                      29097.517658_r_def ]
      self%vertex_coords (:,30) = [  5541.702066_r_def,  12108.839925_r_def, &
                                      29097.517658_r_def ]
      self%vertex_coords (:,31) = [ -7195.043052_r_def, -11205.615628_r_def, &
                                      29097.517658_r_def ]
      self%vertex_coords (:,32) = [  5541.702066_r_def, -12108.839925_r_def, &
                                      29097.517658_r_def ]

      !=========================================================
      ! Assign [x,y,z] cell centre coords in (m), with [0,0,0] at centre
      ! of planet (radius=30000.0).
      ! layer 1
      self%cell_centre_coords (:, 1) = [   155.116303_r_def, &
                                      -2187.360125_r_def, -30922.344585_r_def ]
      self%cell_centre_coords (:, 2) = [  2192.853252_r_def, &
                                     -30922.344585_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:, 3) = [   155.116303_r_def, &
                                      -2187.360125_r_def,  30922.344585_r_def ]
      self%cell_centre_coords (:, 4) = [  2192.853252_r_def, &
                                          0.000000_r_def, -30922.344585_r_def ]
      self%cell_centre_coords (:, 5) = [ 31000.000000_r_def, &
                                          0.000000_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:, 6) = [  2192.853252_r_def, &
                                          0.000000_r_def,  30922.344585_r_def ]
      self%cell_centre_coords (:, 7) = [   155.116303_r_def, &
                                       2187.360125_r_def, -30922.344585_r_def ]
      self%cell_centre_coords (:, 8) = [  2192.853252_r_def, &
                                      30922.344585_r_def,      0.000000_r_def ]
      self%cell_centre_coords (:, 9) = [   155.116303_r_def, &
                                       2187.360125_r_def,  30922.344585_r_def ]

      ! Domain limits
      global_domain_extents(:,1) = [  0.0_r_def,    -0.5_r_def*PI ]
      global_domain_extents(:,2) = [  2.0_r_def*PI, -0.5_r_def*PI ]
      global_domain_extents(:,3) = [  2.0_r_def*PI,  0.5_r_def*PI ]
      global_domain_extents(:,4) = [  0.0_r_def,    -0.5_r_def*PI ]

      self%domain = domain_type( global_domain_extents,   &
                                 self%domain_base_height, &
                                 self%domain_depth,       &
                                 ll_coords )

    else if ( mesh_cfg == PLANE_TWOD_BI_PERIODIC ) then
      !=========================================================
      ! Assign 3D cell local ids on adjacent to given cell
      !
      ! Index ordering follows:
      ! 1) West
      ! 2) South
      ! 3) East
      ! 4) North
      ! 5) Bottom
      ! 6) Top
      !=========================================================
      ! Layer 1
      self%cell_next(:, 1) = [ 3,  7,  2,  4,  0,  0]
      self%cell_next(:, 2) = [ 1,  8,  3,  5,  0,  0]
      self%cell_next(:, 3) = [ 2,  9,  1,  6,  0,  0]
      self%cell_next(:, 4) = [ 6,  1,  5,  7,  0,  0]
      self%cell_next(:, 5) = [ 4,  2,  6,  8,  0,  0]
      self%cell_next(:, 6) = [ 5,  3,  4,  9,  0,  0]
      self%cell_next(:, 7) = [ 9,  4,  8,  1,  0,  0]
      self%cell_next(:, 8) = [ 7,  5,  9,  2,  0,  0]
      self%cell_next(:, 9) = [ 8,  6,  7,  3,  0,  0]

      !=========================================================
      ! Assign vertex local ids on cell corners
      !
      ! Index ordering follows:
      ! 1) South-West Bottom
      ! 2) South-East Bottom
      ! 3) North-East Bottom
      ! 4) North-West Bottom
      ! 5) South-West Top
      ! 6) South-East Top
      ! 7) North-East Top
      ! 8) North-West Top
      !=========================================================
      ! Layer 1
      self%vert_on_cell(:, 1) = [ 1,  2,  3,  4, 10, 11, 12, 13]
      self%vert_on_cell(:, 2) = [ 2,  5,  6,  3, 11, 14, 15, 12]
      self%vert_on_cell(:, 3) = [ 5,  1,  4,  6, 14, 10, 13, 15]
      self%vert_on_cell(:, 4) = [ 4,  3,  7,  8, 13, 12, 16, 17]
      self%vert_on_cell(:, 5) = [ 3,  6,  9,  7, 12, 15, 18, 16]
      self%vert_on_cell(:, 6) = [ 6,  4,  8,  9, 15, 13, 17, 18]
      self%vert_on_cell(:, 7) = [ 8,  7,  2,  1, 17, 16, 11, 10]
      self%vert_on_cell(:, 8) = [ 7,  9,  5,  2, 16, 18, 14, 11]
      self%vert_on_cell(:, 9) = [ 9,  8,  1,  5, 18, 17, 10, 14]

      !=========================================================
      ! Assign edge local ids on cell edges
      !
      ! Index ordering follows:
      ! 1)  West  Bottom
      ! 2)  South Bottom
      ! 3)  East  Bottom
      ! 4)  North Bottom
      ! 5)  South-West Middle
      ! 6)  South-East Middle
      ! 7)  North-East Middle
      ! 8)  North-West Middle
      ! 9)  West  Top
      ! 10) South Top
      ! 11) East  Top
      ! 12) North Top
      !=========================================================
      ! Layer 1
      self%edge_on_cell(:,1) = [ 1,  3,  5,  7,  9, 10, 11, 12,  2,  4,  6,  8]
      self%edge_on_cell(:,2) = [ 5, 13, 15, 17, 10, 19, 20, 11,  6, 14, 16, 18]
      self%edge_on_cell(:,3) = [15, 21,  1, 23, 19,  9, 12, 20, 16, 22,  2, 24]
      self%edge_on_cell(:,4) = [25,  7, 27, 29, 12, 11, 31, 32, 26,  8, 28, 30]
      self%edge_on_cell(:,5) = [27, 17, 33, 35, 11, 20, 37, 31, 28, 18, 34, 36]
      self%edge_on_cell(:,6) = [33, 23, 25, 38, 20, 12, 32, 37, 34, 24, 26, 39]
      self%edge_on_cell(:,7) = [40, 29, 42,  3, 32, 31, 10,  9, 41, 30, 43,  4]
      self%edge_on_cell(:,8) = [42, 35, 44, 13, 31, 37, 19, 10, 43, 36, 45, 14]
      self%edge_on_cell(:,9) = [44, 38, 40, 21, 37, 32,  9, 19, 45, 39, 41, 22]

      !=========================================================
      ! Assign face local ids on cell sides
      !
      ! Index ordering follows:
      ! 1)  West
      ! 2)  South
      ! 3)  East
      ! 4)  North
      ! 5)  Bottom
      ! 6)  Top
      !=========================================================
      self%face_on_cell(:,1) = [ 1,  2,  3,  4,  5,  6]
      self%face_on_cell(:,2) = [ 3,  7,  8,  9, 10, 11]
      self%face_on_cell(:,3) = [ 8, 12,  1, 13, 14, 15]
      self%face_on_cell(:,4) = [16,  4, 17, 18, 19, 20]
      self%face_on_cell(:,5) = [17,  9, 21, 22, 23, 24]
      self%face_on_cell(:,6) = [21, 13, 16, 25, 26, 27]
      self%face_on_cell(:,7) = [28, 18, 29,  2, 30, 31]
      self%face_on_cell(:,8) = [29, 22, 32,  7, 33, 34]
      self%face_on_cell(:,9) = [32, 25, 28, 12, 35, 36]

      ! Domain limits
      global_domain_extents(:,1) = [ -15.0_r_def, -15.0_r_def ]
      global_domain_extents(:,2) = [  15.0_r_def, -15.0_r_def ]
      global_domain_extents(:,3) = [  15.0_r_def,  15.0_r_def ]
      global_domain_extents(:,4) = [ -15.0_r_def,  15.0_r_def ]

      self%domain = domain_type( global_domain_extents,   &
                                 self%domain_base_height, &
                                 self%domain_depth,       &
                                 ll_coords )

    end if

    ! Calculate layer depth dz for flat planet surface
    call set_dz( self%dz,                            &
                 self%eta,                           &
                 self%nlayers,                       &
                 self%domain%minimum_lonlat(axis=3), &
                 self%domain%maximum_lonlat(axis=3) )

    if (.not. allocated(self%mesh_maps)) &
        allocate ( self%mesh_maps,       &
                   source = mesh_map_collection_type() )

    if ( .not. allocated( self%face_id_in_adjacent_cell ) ) &
        allocate ( self%face_id_in_adjacent_cell(4, self%ncells_2d) )

    call calc_face_id_in_adjacent_cell( self%face_id_in_adjacent_cell, &
                                        4, &
                                        self%cell_next, &
                                        6, &
                                        self%ncells_2d )

  end function mesh_constructor_unit_test_data

end module mesh_mod
