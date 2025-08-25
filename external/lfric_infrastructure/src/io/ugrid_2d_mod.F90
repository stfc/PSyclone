!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief   Store 2-dimensional ugrid mesh data.
!> @details Holds all information necessary to define ugrid vn0.9 compliant
!>          storage of 2-dimensional meshes. Pulling data out is currently
!>          done with accessor routines; this may change as dynamo matures.
!-------------------------------------------------------------------------------

module ugrid_2d_mod

use constants_mod,  only: i_def, r_def, str_def, str_longlong, l_def, &
                          imdi, rmdi, cmdi
use file_mod,       only: file_mode_write
use ugrid_file_mod, only: ugrid_file_type


use local_mesh_map_collection_mod,  only: local_mesh_map_collection_type
use global_mesh_map_collection_mod, only: global_mesh_map_collection_type

implicit none

private

!-------------------------------------------------------------------------------
! Module parameters.
!-------------------------------------------------------------------------------

integer(i_def), parameter :: TOPOLOGY_DIMENSION  = 2

!-------------------------------------------------------------------------------
!> @brief Stores 2-dimensional grid information.
!-------------------------------------------------------------------------------
type, public :: ugrid_2d_type
  private

  character(str_def) :: mesh_name

  character(str_def) :: geometry
  character(str_def) :: topology
  character(str_def) :: coord_sys

  integer(i_def) :: npanels = imdi

  logical(l_def) :: periodic_xy(2) = .false.    !< Periodic in x/y axes.

  integer(i_def) :: max_stencil_depth = 0_i_def
  integer(i_def) :: void_cell = 0_i_def

  character(str_longlong) :: constructor_inputs !< Inputs used to generate mesh.

  character(str_def) :: coord_units_xy(2)   = cmdi
  real(r_def)        :: domain_extents(2,4) = rmdi !< Principal coordinates that
                                                   !< describe the domain shape.

  integer(i_def) :: edge_cells_xy(2) !< Number of cells on panel edge x/y-axes

  ! Numbers of different entities.
  integer(i_def) :: num_cells                !< Number of cells.
  integer(i_def) :: num_nodes                !< Number of nodes.
  integer(i_def) :: num_edges                !< Number of edges.
  integer(i_def) :: num_faces                !< Number of faces.

  integer(i_def) :: num_nodes_per_face       !< Number of nodes surrounding each face.
  integer(i_def) :: num_nodes_per_edge       !< Number of nodes defining each edge.
  integer(i_def) :: num_edges_per_face       !< Number of edges bordering each face.
  integer(i_def) :: max_num_faces_per_node   !< Maximum number of faces surrounding each node.

  ! Variables for LBC mesh only.
  integer(i_def) :: rim_depth = imdi         !< Depth (in cells) of rim in LBC meshes

  ! Variables for Local meshes only.
  character(str_def) :: partition_of = cmdi  !< For local meshes, this is the name of
                                             !< the global mesh the local mesh is a
                                             !< partition of.

  integer(i_def) :: inner_depth      = imdi  !< Depth (in cells) of partition inner region.
  integer(i_def) :: halo_depth       = imdi  !< Depth (in cells) of halo region.
  integer(i_def) :: num_edge         = imdi  !< Number of cells in partition edge layer.
  integer(i_def) :: last_edge_cell   = imdi  !< Local ID of last cell in edge layer.
  integer(i_def) :: num_ghost        = imdi  !< Number of cells in partition ghost layer.
  integer(i_def) :: last_ghost_cell  = imdi  !< Local ID of last cell in ghost layer.
  integer(i_def) :: num_faces_global = imdi  !< Number of faces on the global mesh.

  integer(i_def), allocatable :: node_cell_owner(:) !< Local ID of cell that "owns" a given node.
  integer(i_def), allocatable :: edge_cell_owner(:) !< Local ID of cell that "owns" a given edge.

  integer(i_def), allocatable :: num_inner(:)       !< Number of cells on each layer of partition inner region.
  integer(i_def), allocatable :: num_halo(:)        !< Number of cells on each layer of partition halo region.
  integer(i_def), allocatable :: last_inner_cell(:) !< Local ID of last cell of each layer of partition inner region.
  integer(i_def), allocatable :: last_halo_cell(:)  !< Local ID of last cell of each layer of partition halo region.

  integer(i_def) :: num_global_cells = imdi    !< Number of global cell IDs referenced by
                                               !< local mesh. Should be ncells + nghost cells.

  integer(i_def), allocatable :: cell_gid(:)            !< LID to GID map.
  integer(i_def), allocatable :: node_on_cell_gid(:,:)  !< Node(GID)-on-cell(LID) connectivity.
  integer(i_def), allocatable :: edge_on_cell_gid(:,:)  !< Ndge(GID)-on-cell(LID) connectivity.

  ! Coordinates
  real(r_def), allocatable :: node_coordinates(:,:) !< Coordinates of nodes
  real(r_def), allocatable :: face_coordinates(:,:) !< Coordinates of faces

  ! Connectivity
  integer(i_def), allocatable :: face_node_connectivity(:,:) !< Nodes belonging to each face
  integer(i_def), allocatable :: face_edge_connectivity(:,:) !< Edges belonging to each face
  integer(i_def), allocatable :: face_face_connectivity(:,:) !< Neighbouring faces of each face
  integer(i_def), allocatable :: edge_node_connectivity(:,:) !< Nodes belonging to each edge

  ! Target mesh map variables
  integer(i_def) :: nmaps = 0 !< Number of mesh maps for this mesh (as source)

  character(str_def), allocatable :: target_mesh_names(:)   !< Target mesh names
  integer(i_def),     allocatable :: target_edge_cells_x(:) !< Target meshes panel edge cells in x-axis
  integer(i_def),     allocatable :: target_edge_cells_y(:) !< Target meshes panel edge cells in y-axis

  ! Global mesh maps
  type(global_mesh_map_collection_type), pointer :: target_global_mesh_maps => null()

  ! Local mesh maps
  type(local_mesh_map_collection_type),  pointer :: target_local_mesh_maps => null()

  ! Information about the domain orientation
  real(r_def) :: north_pole(2)        !< [Longitude, Latitude] of north pole used
                                      !< for the domain orientation (degrees)
  real(r_def) :: null_island(2)       !< [Longitude, Latitude] of null island
                                      !< used for the domain orientation (degrees)
  real(r_def) :: equatorial_latitude  !< Latitude of equator of mesh (degrees)

  ! File handler
  class(ugrid_file_type), allocatable :: file_handler

contains
  procedure :: get_n_meshes
  procedure :: get_mesh_names
  procedure :: get_dimensions
  procedure :: set_by_generator
  procedure :: set_file_handler
  procedure :: set_from_file_read
  procedure :: write_to_file
  procedure :: append_to_file
  procedure :: get_metadata
  procedure :: get_coord_units
  procedure :: get_node_coords
  procedure :: get_face_coords
  procedure :: get_face_node_connectivity
  procedure :: get_face_edge_connectivity
  procedure :: get_face_face_connectivity
  procedure :: get_edge_node_connectivity
  procedure :: write_coordinates

  procedure :: set_global_mesh_maps
  procedure :: set_local_mesh_maps
  generic   :: set_mesh_maps => set_global_mesh_maps, &
                                set_local_mesh_maps
  procedure :: get_global_mesh_maps
  generic   :: get_mesh_maps => get_global_mesh_maps

  procedure :: get_partition_data

  !> @todo The following routines allow variables to bet set in
  !> ugrid_2d objects. This is pragmatic in order to
  !> avoid circular dependencies. Restructuring work should
  !> remove these if possble.
  procedure :: set_dimensions
  procedure :: set_coords
  procedure :: set_connectivity
  procedure :: set_partition_data
  procedure :: set_metadata

  procedure :: is_local

  !> Routine to destroy object
  procedure :: clear

  !> Object finalizer
  final     :: ugrid_2d_destructor

end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines.
!-------------------------------------------------------------------------------
contains

!-------------------------------------------------------------------------------
!>  @brief Gets number of nodes, edges, faces etc.
!>
!>  @param[out]     num_nodes               Number of nodes.
!>  @param[out]     num_edges               Number of edges.
!>  @param[out]     num_faces               Number of faces.
!>  @param[out]     num_nodes_per_face      Number of nodes around each face.
!>  @param[out]     num_edges_per_face      Number of edges around each face.
!>  @param[out]     num_nodes_per_edge      Number of nodes defining each edge.
!>  @param[out]     max_num_faces_per_node  Maximum number of faces surrounding each node.
!-------------------------------------------------------------------------------

subroutine get_dimensions( self, num_nodes, num_edges, num_faces,  &
                           num_nodes_per_face, num_edges_per_face, &
                           num_nodes_per_edge, max_num_faces_per_node )

  implicit none

  ! Arguments
  class(ugrid_2d_type), intent(in) :: self

  integer(i_def), intent(out) :: num_nodes
  integer(i_def), intent(out) :: num_edges
  integer(i_def), intent(out) :: num_faces
  integer(i_def), intent(out) :: num_nodes_per_face
  integer(i_def), intent(out) :: num_edges_per_face
  integer(i_def), intent(out) :: num_nodes_per_edge
  integer(i_def), intent(out) :: max_num_faces_per_node

  num_nodes = self%num_nodes
  num_edges = self%num_edges
  num_faces = self%num_faces

  num_nodes_per_face = self%num_nodes_per_face
  num_edges_per_face = self%num_edges_per_face
  num_nodes_per_edge = self%num_nodes_per_edge
  max_num_faces_per_node = self%max_num_faces_per_node

  return
end subroutine get_dimensions

!-------------------------------------------------------------------------------
!> @brief Gets a list of the mesh names in a ugrid file.
!>
!> @param[in]  filename   The name of the file to query.
!> @param[out] mesh_names Character[:], Name of mesh topologies in the file.
!-------------------------------------------------------------------------------
subroutine get_mesh_names(self, filename, mesh_names)

  implicit none

  class(ugrid_2d_type), intent(inout) :: self
  character(len=*),     intent(in)    :: filename
  character(len=*),     intent(out)   :: mesh_names(:)

  call self%file_handler%file_open(trim(filename))
  call self%file_handler%get_mesh_names(mesh_names)
  call self%file_handler%file_close()

  return
end subroutine get_mesh_names

!-------------------------------------------------------------------------------
!> @brief Gets the number of the mesh topologies in a ugrid file.
!>
!> @param[in]  filename   The name of the file to query.
!> @param[out] n_meshes   Integer, Number of mesh topologies in the file.
!-------------------------------------------------------------------------------
subroutine get_n_meshes(self, filename, n_meshes)

  implicit none

  class(ugrid_2d_type), intent(inout) :: self
  character(len=*),     intent(in)    :: filename
  integer(i_def),       intent(out)   :: n_meshes

  call self%file_handler%file_open(trim(filename))
  n_meshes = self%file_handler%get_n_meshes()
  call self%file_handler%file_close()

  return
end subroutine get_n_meshes

!-------------------------------------------------------------------------------
!>  @brief   Allocates ugrid_2d internal storage, populated by ugrid generator.
!>  @details Allocates component arrays according to sizes obtained from the
!>           passed generator strategy.
!>
!>  @param[in]  generator_strategy The generator strategy in use.
!-------------------------------------------------------------------------------

subroutine allocate_arrays(self, generator_strategy)
  use ugrid_generator_mod, only: ugrid_generator_type
  implicit none

  ! Arguments
  type(ugrid_2d_type),         intent(inout) :: self
  class(ugrid_generator_type), intent(in)    :: generator_strategy

  call generator_strategy%get_dimensions(                      &
         num_nodes              = self%num_nodes,              &
         num_edges              = self%num_edges,              &
         num_faces              = self%num_faces,              &
         num_nodes_per_face     = self%num_nodes_per_face,     &
         num_edges_per_face     = self%num_edges_per_face,     &
         num_nodes_per_edge     = self%num_nodes_per_edge,     &
         max_num_faces_per_node = self%max_num_faces_per_node )



  if ( allocated( self%node_coordinates ) )       &
                                     deallocate( self%node_coordinates )
  if ( allocated( self%face_coordinates ) )       &
                                     deallocate( self%face_coordinates )

  if ( allocated( self%edge_node_connectivity ) ) &
                                     deallocate( self%edge_node_connectivity )

  if ( allocated( self%face_node_connectivity ) ) &
                                     deallocate( self%face_node_connectivity )

  if ( allocated( self%face_edge_connectivity ) ) &
                                     deallocate( self%face_edge_connectivity )

  if ( allocated( self%face_face_connectivity ) ) &
                                     deallocate( self%face_face_connectivity )

  allocate(self%node_coordinates(2, self%num_nodes))
  allocate(self%face_coordinates(2, self%num_faces))
  allocate(self%edge_node_connectivity(self%num_nodes_per_edge, self%num_edges))
  allocate(self%face_node_connectivity(self%num_nodes_per_face, self%num_faces))
  allocate(self%face_edge_connectivity(self%num_edges_per_face, self%num_faces))
  allocate(self%face_face_connectivity(self%num_edges_per_face, self%num_faces))

  if (self%nmaps > 0) then
    if (allocated(self%target_mesh_names))   deallocate (self%target_mesh_names)
    if (allocated(self%target_edge_cells_x)) deallocate (self%target_edge_cells_x)
    if (allocated(self%target_edge_cells_y)) deallocate (self%target_edge_cells_y)

    allocate(self%target_mesh_names(self%nmaps))
    allocate(self%target_edge_cells_x(self%nmaps))
    allocate(self%target_edge_cells_y(self%nmaps))
  end if

  return
end subroutine allocate_arrays

!-------------------------------------------------------------------------------
!>  @brief   Allocates ugrid_2d internal storage, populated by ugrid file.
!>  @details Allocates component arrays according to sizes already stored in
!>           the ugrid_2d object. These arrays are populated elsewhere
!>           by a ugrid file strategy.
!>
!-------------------------------------------------------------------------------

subroutine allocate_arrays_for_file(self)
  implicit none

  ! Arguments
  type(ugrid_2d_type),    intent(inout) :: self

  if ( allocated( self%node_coordinates ) )       &
                                     deallocate( self%node_coordinates )
  if ( allocated( self%face_coordinates ) )       &
                                     deallocate( self%face_coordinates )

  if ( allocated( self%edge_node_connectivity ) ) &
                                     deallocate( self%edge_node_connectivity )

  if ( allocated( self%face_node_connectivity ) ) &
                                     deallocate( self%face_node_connectivity )

  if ( allocated( self%face_edge_connectivity ) ) &
                                     deallocate( self%face_edge_connectivity )

  if ( allocated( self%face_face_connectivity ) ) &
                                     deallocate( self%face_face_connectivity )

  if ( allocated( self%target_mesh_names ) ) &
                                     deallocate( self%target_mesh_names )

  allocate(self%node_coordinates(2, self%num_nodes))
  allocate(self%face_coordinates(2, self%num_faces))

  allocate(self%edge_node_connectivity(self%num_nodes_per_edge, self%num_edges))
  allocate(self%face_node_connectivity(self%num_nodes_per_face, self%num_faces))
  allocate(self%face_edge_connectivity(self%num_edges_per_face, self%num_faces))
  allocate(self%face_face_connectivity(self%num_edges_per_face, self%num_faces))

  if (self%nmaps > 0) then
    allocate( self%target_mesh_names(self%nmaps) )
  end if

  return
end subroutine allocate_arrays_for_file

!---------------------------------------------------------------------------------
!>  @brief   Populate arrays according to passed generator strategy.
!>  @details Calls back to the passed generator strategy in order to populate
!>           the coordinate and connectivity arrays.
!>
!>  @param[in,out] generator_strategy The generator with which to generate the mesh.
!---------------------------------------------------------------------------------
subroutine set_by_generator(self, generator_strategy)

  use ugrid_generator_mod, only: ugrid_generator_type

  implicit none

  class(ugrid_2d_type),        intent(inout) :: self
  class(ugrid_generator_type), intent(inout) :: generator_strategy

  call generator_strategy%generate()

  call generator_strategy%get_metadata                  &
      ( mesh_name           = self%mesh_name,           &
        geometry            = self%geometry,            &
        topology            = self%topology,            &
        coord_sys           = self%coord_sys,           &
        periodic_xy         = self%periodic_xy,         &
        edge_cells_x        = self%edge_cells_xy(1),    &
        edge_cells_y        = self%edge_cells_xy(2),    &
        constructor_inputs  = self%constructor_inputs,  &
        north_pole          = self%north_pole,          &
        null_island         = self%null_island,         &
        equatorial_latitude = self%equatorial_latitude, &
        rim_depth           = self%rim_depth,           &
        nmaps               = self%nmaps,               &
        void_cell           = self%void_cell )

  self%npanels = generator_strategy%get_number_of_panels()

  call allocate_arrays(self, generator_strategy)

  if (self%nmaps > 0) then
    call generator_strategy%get_metadata                &
        ( target_mesh_names = self%target_mesh_names,   &
          maps_edge_cells_x = self%target_edge_cells_x, &
          maps_edge_cells_y = self%target_edge_cells_y )

    self%target_global_mesh_maps => generator_strategy%get_global_mesh_maps()
  end if

  call generator_strategy%get_coordinates          &
      ( node_coordinates = self%node_coordinates,  &
        cell_coordinates = self%face_coordinates,  &
        domain_extents   = self%domain_extents,    &
        coord_units_x    = self%coord_units_xy(1), &
        coord_units_y    = self%coord_units_xy(2) )

  call generator_strategy%get_connectivity                    &
      ( face_node_connectivity = self%face_node_connectivity, &
        edge_node_connectivity = self%edge_node_connectivity, &
        face_edge_connectivity = self%face_edge_connectivity, &
        face_face_connectivity = self%face_face_connectivity )

  return
end subroutine set_by_generator

!-------------------------------------------------------------------------------
!> @brief   Sets the file read/write strategy.
!> @details Receives a file-handler object and moves its allocation into
!>          the appropriate type component. On exit, the file_handler
!>          dummy argument will no longer be allocated.
!>
!> @param[in,out] file_handler The file handler to use for IO.
!-------------------------------------------------------------------------------

subroutine set_file_handler(self, file_handler)
  implicit none

  ! Arguments
  class(ugrid_2d_type),                intent(inout) :: self
  class(ugrid_file_type), allocatable, intent(inout) :: file_handler

  call move_alloc(file_handler, self%file_handler)

  return
end subroutine set_file_handler

!-------------------------------------------------------------------------------
!> @brief   Reads ugrid information and populates internal arrays.
!> @details Calls back to the file handler strategy (component) in order to
!>          read the ugrid mesh data and populate internal arrays with data
!>          from a file.
!>
!> @param[in]     mesh_name Name of the mesh topology.
!> @param[in]     filename  Name of the ugrid file.
!-------------------------------------------------------------------------------

subroutine set_from_file_read(self, mesh_name, filename)

  implicit none

  ! Arguments
  class(ugrid_2d_type), intent(inout) :: self
  character(len=*),     intent(in)    :: mesh_name
  character(len=*),     intent(in)    :: filename

  self%mesh_name = trim(mesh_name)

  call self%file_handler%file_open(trim(filename))

  call self%file_handler%get_dimensions(                  &
       mesh_name              = self%mesh_name,           &
       num_nodes              = self%num_nodes,           &
       num_edges              = self%num_edges,           &
       num_faces              = self%num_faces,           &
       num_nodes_per_face     = self%num_nodes_per_face,  &
       num_edges_per_face     = self%num_edges_per_face,  &
       num_nodes_per_edge     = self%num_nodes_per_edge,  &
       max_num_faces_per_node = self%max_num_faces_per_node )

  call allocate_arrays_for_file(self)

  call self%file_handler%read_mesh(                                 &
       self%mesh_name, self%geometry, self%coord_sys,               &
       self%north_pole, self%null_island, self%equatorial_latitude, &
       self%node_coordinates, self%face_coordinates,                &
       self%coord_units_xy(1), self%coord_units_xy(2),              &
       self%void_cell,                                              &
       self%face_node_connectivity, self%face_edge_connectivity,    &
       self%face_face_connectivity, self%edge_node_connectivity,    &
       self%topology, self%periodic_xy, self%domain_extents,        &
       self%npanels, self%rim_depth, self%constructor_inputs,       &

       self%partition_of, self%num_faces_global,                    &
       self%max_stencil_depth,                                      &
       self%inner_depth, self%num_inner, self%last_inner_cell,      &
       self%halo_depth,  self%num_halo,  self%last_halo_cell,       &
       self%num_edge,  self%last_edge_cell,                         &
       self%num_ghost, self%last_ghost_cell,                        &
       self%node_cell_owner, self%edge_cell_owner,                  &
       self%cell_gid, self%node_on_cell_gid,                        &
       self%edge_on_cell_gid,                                       &

       self%nmaps, self%target_mesh_names )


  call self%file_handler%file_close()

  return
end subroutine set_from_file_read


!-------------------------------------------------------------------------------
!> @brief   Writes stored ugrid information to data file.
!> @details Calls back to the file handler strategy (component) in order to
!>          read the ugrid mesh data and populate internal arrays with data
!>          from a file.
!>
!> @param[in] filename  Filename to write contents to.
!-------------------------------------------------------------------------------
subroutine write_to_file(self, filename)

  use ugrid_generator_mod, only: ugrid_generator_type

  implicit none

  ! Arguments
  class(ugrid_2d_type), intent(inout) :: self
  character(len=*),     intent(in)    :: filename

  call self%file_handler%file_new(trim(filename))

  call self%file_handler%write_mesh(                         &

       ! Common mesh type variables.
       mesh_name              = self%mesh_name,              &
       geometry               = self%geometry,               &
       coord_sys              = self%coord_sys,              &
       north_pole             = self%north_pole,             &
       null_island            = self%null_island,            &
       equatorial_latitude    = self%equatorial_latitude,    &
       num_nodes              = self%num_nodes,              &
       num_edges              = self%num_edges,              &
       num_faces              = self%num_faces,              &
       node_coordinates       = self%node_coordinates,       &
       face_coordinates       = self%face_coordinates,       &
       coord_units_x          = self%coord_units_xy(1),      &
       coord_units_y          = self%coord_units_xy(2),      &
       void_cell              = self%void_cell,              &
       face_node_connectivity = self%face_node_connectivity, &
       face_edge_connectivity = self%face_edge_connectivity, &
       face_face_connectivity = self%face_face_connectivity, &
       edge_node_connectivity = self%edge_node_connectivity, &

       ! Global mesh variables.
       topology           = self%topology,           &
       periodic_xy        = self%periodic_xy,        &
       domain_extents     = self%domain_extents,     &
       npanels            = self%npanels,            &
       rim_depth          = self%rim_depth,          &
       constructor_inputs = self%constructor_inputs, &

       ! Partition variables.
       partition_of       = self%partition_of,       &
       num_faces_global   = self%num_faces_global,   &
       max_stencil_depth  = self%max_stencil_depth,  &
       inner_depth        = self%inner_depth,        &
       num_inner          = self%num_inner,          &
       last_inner_cell    = self%last_inner_cell,    &
       halo_depth         = self%halo_depth,         &
       num_halo           = self%num_halo,           &
       last_halo_cell     = self%last_halo_cell,     &
       num_edge           = self%num_edge,           &
       last_edge_cell     = self%last_edge_cell,     &
       num_ghost          = self%num_ghost,          &
       last_ghost_cell    = self%last_ghost_cell,    &
       node_cell_owner    = self%node_cell_owner,    &
       edge_cell_owner    = self%edge_cell_owner,    &
       cell_gid           = self%cell_gid,           &
       node_on_cell_gid   = self%node_on_cell_gid,   &
       edge_on_cell_gid   = self%edge_on_cell_gid,   &

       ! Inter-grid maps.
       num_targets             = self%nmaps,                   &
       target_mesh_names       = self%target_mesh_names,       &
       target_global_mesh_maps = self%target_global_mesh_maps, &
       target_local_mesh_maps  = self%target_local_mesh_maps )

  call self%file_handler%file_close()

  return
end subroutine write_to_file


!-------------------------------------------------------------------------------
!> @brief   Appends stored ugrid information to existing ugrid data file.
!> @details Calls back to the file handler strategy (component) in order to
!>          read the ugrid_2d_type mesh data and populate the file_handlers
!>          internal arrays which the file handler will append to the ugrid file.
!>
!> @param[in] filename  Output file to open to add data.
!-------------------------------------------------------------------------------
subroutine append_to_file(self, filename)

  use ugrid_generator_mod, only: ugrid_generator_type

  implicit none

  ! Arguments
  class(ugrid_2d_type), intent(inout) :: self
  character(len=*),     intent(in)    :: filename

  call self%file_handler%file_open( trim(filename), &
                                    file_mode=file_mode_write)

  call self%file_handler%append_mesh(                        &

       ! Common mesh type variables.
       mesh_name              = self%mesh_name,              &
       geometry               = self%geometry,               &
       coord_sys              = self%coord_sys,              &
       north_pole             = self%north_pole,             &
       null_island            = self%null_island,            &
       equatorial_latitude    = self%equatorial_latitude,    &
       num_nodes              = self%num_nodes,              &
       num_edges              = self%num_edges,              &
       num_faces              = self%num_faces,              &
       node_coordinates       = self%node_coordinates,       &
       face_coordinates       = self%face_coordinates,       &
       coord_units_x          = self%coord_units_xy(1),      &
       coord_units_y          = self%coord_units_xy(2),      &
       void_cell              = self%void_cell,              &
       face_node_connectivity = self%face_node_connectivity, &
       face_edge_connectivity = self%face_edge_connectivity, &
       face_face_connectivity = self%face_face_connectivity, &
       edge_node_connectivity = self%edge_node_connectivity, &

       ! Global mesh variables.
       topology           = self%topology,           &
       periodic_xy        = self%periodic_xy,        &
       domain_extents     = self%domain_extents,     &
       npanels            = self%npanels,            &
       rim_depth          = self%rim_depth,          &
       constructor_inputs = self%constructor_inputs, &

       ! Partition variables.
       partition_of       = self%partition_of,       &
       num_faces_global   = self%num_faces_global,   &
       max_stencil_depth  = self%max_stencil_depth,  &
       inner_depth        = self%inner_depth,        &
       num_inner          = self%num_inner,          &
       last_inner_cell    = self%last_inner_cell,    &
       halo_depth         = self%halo_depth,         &
       num_halo           = self%num_halo,           &
       last_halo_cell     = self%last_halo_cell,     &
       num_edge           = self%num_edge,           &
       last_edge_cell     = self%last_edge_cell,     &
       num_ghost          = self%num_ghost,          &
       last_ghost_cell    = self%last_ghost_cell,    &
       node_cell_owner    = self%node_cell_owner,    &
       edge_cell_owner    = self%edge_cell_owner,    &
       cell_gid           = self%cell_gid,           &
       node_on_cell_gid   = self%node_on_cell_gid,   &
       edge_on_cell_gid   = self%edge_on_cell_gid,   &

       ! Intergrid maps
       num_targets             = self%nmaps,                   &
       target_mesh_names       = self%target_mesh_names,       &
       target_global_mesh_maps = self%target_global_mesh_maps, &
       target_local_mesh_maps  = self%target_local_mesh_maps )

  call self%file_handler%file_close()

  return
end subroutine append_to_file


!-------------------------------------------------------------------------------
!> @brief    Gets metadata of the current mesh in this object which was
!>           set by the ugrid_generator_type or read in from NetCDF (UGRID) file.
!> @details  The ugrid_2d object may hold a global or local mesh of differing
!>           topologies. As such, some of the metadata from this routine
!>           will not apply in all cases.
!>
!> @param[out] mesh_name          Name of the current mesh topology.
!> @param[out] geometry           Domain geometry enumeration key.
!> @param[out] topology           Domain topology enumeration key.
!> @param[out] coord_sys          Co-ordinate sys enumeration key.
!> @param[out] npanels            Number of panels used in mesh topology.
!> @param[out] periodic_xy        Periodic in x/y-axes.
!> @param[out] max_stencil_depth  Maximum stencil depth supported (Local meshes).
!> @param[out] void_cell          Values to mark a cell as external
!>                                to domain.
!> @param[out] edge_cells_x       Number of panel edge cells (x-axis).
!> @param[out] edge_cells_y       Number of panel edge cells (y-axis).
!> @param[out] ncells_global      Number of cells in the global mesh.
!> @param[out] domain_extents     Principal coordinates that describe the
!>                                domain shape.
!> @param[out] rim_depth          Rim depth (in cells) for LBC meshes.
!> @param[out] inner_depth        Number of inner halo layers.
!> @param[out] halo_depth         Number of outer halo layers.
!> @param[out] partition_of       Specifies the global mesh name, if this mesh a local mesh.
!> @param[out] num_edge           Number of cells in the edge layer of the partition.
!> @param[out] last_edge_cell     Array index of the last cell in the edge layer.
!> @param[out] num_ghost          Number of cells in the ghost layer of the partition.
!> @param[out] last_ghost_cell    Array index of the last cell in the ghost layer.
!> @param[out] constructor_inputs Input arguments use to create this mesh.
!> @param[out] nmaps              The number of intergrid maps from this mesh.
!> @param[out] target_mesh_names  Names of target mesh topologies in this file
!>                                which this mesh possesses cell-cell maps for.
!> @param[out] north_pole         Optional, [Longitude, Latitude] of north pole
!>                                used for domain orientation (degrees).
!> @param[out] null_island        Optional, [Longitude, Latitude] of null
!>                                island used for domain orientation (degrees).
!> @param[out] equatorial_latitude Optional, latitude of equator of mesh.
!-------------------------------------------------------------------------------
subroutine get_metadata( self, mesh_name,               &
                         geometry, topology, coord_sys, &
                         npanels, periodic_xy,          &
                         max_stencil_depth, void_cell,  &
                         edge_cells_x, edge_cells_y,    &
                         ncells_global, domain_extents, &
                         rim_depth, inner_depth,        &
                         halo_depth, partition_of,      &
                         num_edge, last_edge_cell,      &
                         num_ghost, last_ghost_cell,    &
                         constructor_inputs, nmaps,     &
                         target_mesh_names,             &
                         north_pole, null_island,       &
                         equatorial_latitude )

  implicit none

  class(ugrid_2d_type), intent(in) :: self
  character(str_def),   optional, intent(out) :: mesh_name
  character(str_def),   optional, intent(out) :: geometry
  character(str_def),   optional, intent(out) :: topology
  character(str_def),   optional, intent(out) :: coord_sys
  integer(i_def),       optional, intent(out) :: npanels
  logical(l_def),       optional, intent(out) :: periodic_xy(2)

  integer(i_def),       optional, intent(out) :: max_stencil_depth
  integer(i_def),       optional, intent(out) :: void_cell
  integer(i_def),       optional, intent(out) :: edge_cells_x
  integer(i_def),       optional, intent(out) :: edge_cells_y

  character(str_longlong),  optional, intent(out) :: constructor_inputs

  integer(i_def),       optional, intent(out) :: nmaps
  character(str_def),   optional, intent(out), &
                                  allocatable :: target_mesh_names(:)

  character(str_def),   optional, intent(out) :: partition_of

  real(r_def),    optional, intent(out) :: north_pole(2)
  real(r_def),    optional, intent(out) :: null_island(2)
  real(r_def),    optional, intent(out) :: equatorial_latitude
  integer(i_def), optional, intent(out) :: inner_depth
  integer(i_def), optional, intent(out) :: halo_depth
  integer(i_def), optional, intent(out) :: num_edge
  integer(i_def), optional, intent(out) :: last_edge_cell
  integer(i_def), optional, intent(out) :: num_ghost
  integer(i_def), optional, intent(out) :: last_ghost_cell
  integer(i_def), optional, intent(out) :: ncells_global
  real(r_def),    optional, intent(out) :: domain_extents(2,4)
  integer(i_def), optional, intent(out) :: rim_depth

  if (present(constructor_inputs))  constructor_inputs  = self%constructor_inputs
  if (present(domain_extents))      domain_extents      = self%domain_extents
  if (present(rim_depth))           rim_depth           = self%rim_depth
  if (present(partition_of))        partition_of        = self%partition_of
  if (present(inner_depth))         inner_depth         = self%inner_depth
  if (present(halo_depth))          halo_depth          = self%halo_depth
  if (present(num_edge))            num_edge            = self%num_edge
  if (present(last_edge_cell))      last_edge_cell      = self%last_edge_cell
  if (present(num_ghost))           num_ghost           = self%num_ghost
  if (present(last_ghost_cell))     last_ghost_cell     = self%last_ghost_cell
  if (present(north_pole))          north_pole          = self%north_pole
  if (present(null_island))         null_island         = self%null_island
  if (present(equatorial_latitude)) equatorial_latitude = self%equatorial_latitude
  if (present(ncells_global))       ncells_global       = self%num_global_cells

  if (present(mesh_name))          mesh_name          = self%mesh_name
  if (present(geometry))           geometry           = self%geometry
  if (present(topology))           topology           = self%topology
  if (present(coord_sys))          coord_sys          = self%coord_sys
  if (present(npanels ))           npanels            = self%npanels
  if (present(periodic_xy))        periodic_xy        = self%periodic_xy
  if (present(max_stencil_depth))  max_stencil_depth  = self%max_stencil_depth
  if (present(void_cell))          void_cell          = self%void_cell
  if (present(constructor_inputs)) constructor_inputs = self%constructor_inputs
  if (present(edge_cells_x))       edge_cells_x       = self%edge_cells_xy(1)
  if (present(edge_cells_y))       edge_cells_y       = self%edge_cells_xy(2)
  if (present(nmaps))              nmaps              = self%nmaps
  if (self%nmaps > 0 .and. present(target_mesh_names)) then
    target_mesh_names = self%target_mesh_names
  end if

end subroutine get_metadata

!-------------------------------------------------------------------------------
!> @brief  Gets node coordinate units in [x,y] or [longitude, latitude] directions.
!>
!> @param[out]  coord_units_xy  String for coordinate units along x/y-axes.
!-------------------------------------------------------------------------------
subroutine get_coord_units(self, coord_units_xy)
  implicit none

  class(ugrid_2d_type), intent(in)  :: self
  character(str_def),   intent(out) :: coord_units_xy(2)

  coord_units_xy = self%coord_units_xy

  return
end subroutine get_coord_units

!-------------------------------------------------------------------------------
!> @brief   Gets node coordinates with ugrid array index ordering.
!> @details Returns a rank-two array of node coordinates, with the
!>          coordinate dimension index innermost, and the node number
!>          outermost. Format: [long, lat, radius].
!>
!> @param[out]  node_coords  Node coordinate array.
!-------------------------------------------------------------------------------

subroutine get_node_coords(self, node_coords)
  implicit none

  class(ugrid_2d_type), intent(in)  :: self

  real(r_def),          intent(out) :: node_coords(:,:)

  integer(i_def) :: i

  do i = 1, self%num_nodes
    node_coords(1:2,i) = self%node_coordinates(1:2,i)
  end do

  return
end subroutine get_node_coords


!-------------------------------------------------------------------------------
!> @brief   Gets face coordinates with ugrid array index ordering.
!> @details Returns a rank-two array of node coordinates, with the
!>          coordinate dimension index innermost, and the face number
!>          outermost. Format: [long, lat].
!>
!> @return  face_coords Face coordinate array.
!-------------------------------------------------------------------------------

function get_face_coords(self) result(face_coords)

  implicit none

  class(ugrid_2d_type), intent(in)  :: self

  real(r_def), allocatable :: face_coords(:,:)

  face_coords = self%face_coordinates(:,:)

  return
end function get_face_coords

!-------------------------------------------------------------------------------
!> @brief   Gets an array of node indices surrounding each face.
!> @details Returns a rank-two array of nodes surrounding each face, with
!>          the nodes surrounding any single face being contiguous.
!>
!> @return  face_node_connectivity  Indices of nodes adjacent to faces.
!-------------------------------------------------------------------------------
function get_face_node_connectivity( self ) &
                             result( face_node_connectivity )
  implicit none

  class(ugrid_2d_type), intent(in) :: self
  integer(i_def), allocatable      :: face_node_connectivity(:,:)

  if ( allocated(face_node_connectivity) ) then
    deallocate(face_node_connectivity)
  end if

  allocate( face_node_connectivity, &
            source=self%face_node_connectivity )
  return
end function get_face_node_connectivity

!-------------------------------------------------------------------------------
!> @brief   Gets an array of edge indices surrounding each face.
!> @details Returns a rank-two array of edges surrounding each face, with
!>          the edges surrounding any single face being contiguous.
!>
!> @return  face_edge_connectivity  Indices of edges adjacent to faces.
!-------------------------------------------------------------------------------
function get_face_edge_connectivity( self ) &
                             result( face_edge_connectivity )
  implicit none

  class(ugrid_2d_type), intent(in) :: self
  integer(i_def), allocatable      :: face_edge_connectivity(:,:)

  if ( allocated(face_edge_connectivity) ) then
    deallocate(face_edge_connectivity)
  end if

  allocate( face_edge_connectivity, &
            source=self%face_edge_connectivity )
  return
end function get_face_edge_connectivity


!-------------------------------------------------------------------------------
!> @brief   Gets an array of face indices surrounding each face.
!> @details Returns a rank-two array of faces surrounding each face, with
!>          the faces surrounding any single face being contiguous.
!>
!> @return  face_face_connectivity  Indices of faces adjacent to faces.
!-------------------------------------------------------------------------------
function get_face_face_connectivity( self ) &
                             result( face_face_connectivity )
  implicit none

  class(ugrid_2d_type), intent(in) :: self
  integer(i_def), allocatable      :: face_face_connectivity(:,:)

  if ( allocated(face_face_connectivity) ) then
    deallocate(face_face_connectivity)
  end if

  allocate( face_face_connectivity, &
            source=self%face_face_connectivity )
  return
end function get_face_face_connectivity


!-------------------------------------------------------------------------------
!> @brief   Gets an array of node indices attached to each edge.
!> @details Returns a rank-two array of nodes attached to each edge.
!>
!> @return  edge_node_connectivity  Indices of nodes attached to edges.
!-------------------------------------------------------------------------------
function get_edge_node_connectivity( self ) &
                             result( edge_node_connectivity )
  implicit none

  class(ugrid_2d_type), intent(in) :: self
  integer(i_def), allocatable      :: edge_node_connectivity(:,:)

  if ( allocated(edge_node_connectivity) ) then
    deallocate(edge_node_connectivity)
  end if

  allocate( edge_node_connectivity, &
            source=self%edge_node_connectivity )
  return
end function get_edge_node_connectivity


!-------------------------------------------------------------------------------
!> @brief   Gets information related to local partition.
!> @details Local meshes (on a paritition) require additional information
!>          regarding the partition and the larger global mesh.
!>
!> param[out] max_stencil_depth  Maximum stencil depth supported.
!> param[out] inner_depth        Number of inner layers.
!> param[out] halo_depth         Number of halo layers.
!> param[out] num_inner          Number cells in each inner layer.
!> param[out] num_edge           Number cells in edge layer.
!> param[out] num_halo           Number cells in each halo layer.
!> param[out] num_ghost          Number cells in ghost layer.
!> param[out] last_inner_cell    Local index of last cell in each inner layer.
!> param[out] last_edge_cell     Local index of last cell in edge layer.
!> param[out] last_halo_cell     Local index of last cell in each halo layer.
!> param[out] last_ghost_cell    Local index of last cell in ghost layer.
!> param[out] node_cell_owner    Cell ID that owns a given node.
!> param[out] edge_cell_owner    Cell ID that owns a given edge.
!> param[out] num_faces_global   Number of faces in the global mesh.
!> param[out] cell_gid           Global index of cells in this local mesh object.
!> param[out] node_on_cell_gid   Node on cell connectivity in Global IDs.
!> param[out] edge_on_cell_gid   Edge on cell connectivity in Global IDs.
!-------------------------------------------------------------------------------
subroutine get_partition_data( self,              &
                               max_stencil_depth, &
                               inner_depth,       &
                               halo_depth,        &
                               num_inner,         &
                               num_edge,          &
                               num_halo,          &
                               num_ghost,         &
                               last_inner_cell,   &
                               last_edge_cell,    &
                               last_halo_cell ,   &
                               last_ghost_cell,   &
                               node_cell_owner,   &
                               edge_cell_owner,   &
                               num_faces_global,  &
                               cell_gid,          &
                               node_on_cell_gid,  &
                               edge_on_cell_gid )

  implicit none

  class (ugrid_2d_type), intent(in) :: self

  integer(i_def), intent(out) :: max_stencil_depth
  integer(i_def), intent(out) :: inner_depth
  integer(i_def), intent(out) :: halo_depth
  integer(i_def), intent(out) :: num_edge
  integer(i_def), intent(out) :: last_edge_cell
  integer(i_def), intent(out) :: num_ghost
  integer(i_def), intent(out) :: last_ghost_cell
  integer(i_def), intent(out) :: num_faces_global

  integer(i_def), intent(out), allocatable :: node_cell_owner(:)
  integer(i_def), intent(out), allocatable :: edge_cell_owner(:)
  integer(i_def), intent(out), allocatable :: num_inner(:)
  integer(i_def), intent(out), allocatable :: num_halo(:)
  integer(i_def), intent(out), allocatable :: last_inner_cell(:)
  integer(i_def), intent(out), allocatable :: last_halo_cell(:)

  integer(i_def), intent(out), allocatable :: cell_gid(:)
  integer(i_def), intent(out), allocatable :: node_on_cell_gid(:,:)
  integer(i_def), intent(out), allocatable :: edge_on_cell_gid(:,:)


  max_stencil_depth = self%max_stencil_depth
  inner_depth       = self%inner_depth
  halo_depth        = self%halo_depth
  num_edge          = self%num_edge
  last_edge_cell    = self%last_edge_cell
  num_ghost         = self%num_ghost
  last_ghost_cell   = self%last_ghost_cell
  num_faces_global  = self%num_faces_global

  if (allocated( node_cell_owner )) deallocate( node_cell_owner )
  if (allocated( edge_cell_owner )) deallocate( edge_cell_owner )
  if (allocated( num_inner ))       deallocate( num_inner )
  if (allocated( num_halo ))        deallocate( num_halo )
  if (allocated( last_inner_cell )) deallocate( last_inner_cell )
  if (allocated( last_halo_cell ))  deallocate( last_halo_cell )

  if (allocated( cell_gid ))         deallocate( cell_gid )
  if (allocated( edge_on_cell_gid )) deallocate( edge_on_cell_gid )
  if (allocated( node_on_cell_gid )) deallocate( node_on_cell_gid )

  if (self%inner_depth > 0) then
    allocate( num_inner,       source=self%num_inner )
    allocate( last_inner_cell, source=self%last_inner_cell )
  end if

  if (self%halo_depth > 0) then
    allocate( num_halo,       source=self%num_halo )
    allocate( last_halo_cell, source=self%last_halo_cell )
  end if

  allocate( node_cell_owner, source=self%node_cell_owner )
  allocate( edge_cell_owner, source=self%edge_cell_owner )
  allocate( cell_gid,         source=self%cell_gid         )
  allocate( node_on_cell_gid, source=self%node_on_cell_gid )
  allocate( edge_on_cell_gid, source=self%edge_on_cell_gid )

end subroutine get_partition_data

!-------------------------------------------------------------------------------
!> @brief     Assigns a global_mesh_map_collection to the ugrid_2d object
!> @param[in] global_maps  Global mesh map collection
!-------------------------------------------------------------------------------
subroutine set_global_mesh_maps( self, global_maps  )

  implicit none

  class (ugrid_2d_type), intent(inout) :: self

  type(global_mesh_map_collection_type), &
                         intent(in), target :: global_maps

  self%target_global_mesh_maps => global_maps

end subroutine set_global_mesh_maps

!-------------------------------------------------------------------------------
!> @brief     Assigns a local_mesh_map_collection to the ugrid_2d object
!> @param[in] local_maps  Local mesh map collection
!-------------------------------------------------------------------------------
subroutine set_local_mesh_maps( self, local_maps )

  implicit none

  class (ugrid_2d_type), intent(inout) :: self

  type(local_mesh_map_collection_type), &
                         intent(in), target :: local_maps

  nullify(self%target_local_mesh_maps)
  self%target_local_mesh_maps => local_maps

end subroutine set_local_mesh_maps


!-------------------------------------------------------------------------------
!> @brief     Retrieves pointer to current global_mesh_map_collection
!>            assigned to the ugrid_2d object.
!> @param[out] global_maps  Pointer to global mesh map collection in ugrid_2d
!>                          object.
!-------------------------------------------------------------------------------
subroutine get_global_mesh_maps( self, global_maps  )

  implicit none

  class (ugrid_2d_type), intent(in), target :: self

  type(global_mesh_map_collection_type), &
                         intent(out), pointer :: global_maps

  global_maps => self%target_global_mesh_maps

end subroutine get_global_mesh_maps


!-------------------------------------------------------------------------------
!> @brief     Assign dimesnsion related variables in the ugrid_2d file.
!>
!> @param[in] num_nodes            Optional: Number of unique nodes on mesh.
!> @param[in] num_edges            Optional: Number of unique edges on mesh.
!> @param[in] num_faces            Optional: Number of unique faces on mesh.
!> @param[in] num_nodes_per_face   Optional: Number of nodes to construct a face
!> @param[in] num_edges_per_face   Optional: Number of edges to construct a face
!> @param[in] num_nodes_per_edge   Optional: Number of nodes to construct an edge
!> @param[in] max_num_of_faces_per_node
!>                                 Optional: Maximum number of faces attached
!>                                           to a given node.
!-------------------------------------------------------------------------------
subroutine set_dimensions( self,       &
               num_nodes,              &
               num_edges,              &
               num_faces,              &
               num_nodes_per_face,     &
               num_edges_per_face,     &
               num_nodes_per_edge,     &
               max_num_faces_per_node )

  implicit none

  class (ugrid_2d_type), intent(inout) :: self

  integer(i_def), optional :: num_nodes
  integer(i_def), optional :: num_edges
  integer(i_def), optional :: num_faces

  integer(i_def), optional :: num_nodes_per_face
  integer(i_def), optional :: num_nodes_per_edge
  integer(i_def), optional :: num_edges_per_face
  integer(i_def), optional :: max_num_faces_per_node

  if (present(num_faces)) then
    self%num_cells = num_faces
    self%num_faces = num_faces
  end if

  if (present(num_nodes)) self%num_nodes = num_nodes
  if (present(num_edges)) self%num_edges = num_edges

  if (present(num_nodes_per_face)) then
    self%num_nodes_per_face = num_nodes_per_face
  end if

  if (present(num_nodes_per_edge)) then
    self%num_nodes_per_edge = num_nodes_per_edge
  end if

  if (present(num_edges_per_face)) then
    self%num_edges_per_face = num_edges_per_face
  end if

  if (present(max_num_faces_per_node)) then
    self%max_num_faces_per_node = max_num_faces_per_node
  end if

end subroutine set_dimensions


!-------------------------------------------------------------------------------
!> @brief     Assign coordinate related variables in the ugrid_2d file.
!>
!> @param[in] node_coords         Optional: Mesh node coords.
!> @param[in] face_coords         Optional: Face centre coords.
!> @param[in] north_pole          Optional: Real world North pole  [lon,lat] coords
!> @param[in] null_island         Optional: Real world Null island [lon,lat] coords
!> @param[in] equatorial_latitude Optional: latitude of equator of mesh
!> @param[in] coord_sys           Optional: Coordinate system used by mesh
!> @param[in] units_xy            Optional: Units in x/y-axes
!-------------------------------------------------------------------------------
subroutine set_coords( self,                     &
                       node_coords, face_coords, &
                       north_pole, null_island,  &
                       equatorial_latitude,      &
                       coord_sys, units_xy )

  implicit none

  class (ugrid_2d_type), intent(inout) :: self

  real(r_def), optional, intent(in) :: node_coords(:,:)
  real(r_def), optional, intent(in) :: face_coords(:,:)
  real(r_def), optional, intent(in) :: north_pole(:)
  real(r_def), optional, intent(in) :: null_island(:)
  real(r_def), optional, intent(in) :: equatorial_latitude

  character(str_def), optional, intent(in) :: coord_sys
  character(str_def), optional, intent(in) :: units_xy(2)

  if (present(node_coords)) then
    if (allocated( self%node_coordinates )) then
      deallocate( self%node_coordinates )
    end if
    allocate( self%node_coordinates, source=node_coords )
  end if

  if (present(face_coords)) then
    if (allocated( self%face_coordinates )) then
      deallocate( self%face_coordinates )
    end if
    allocate( self%face_coordinates, source=face_coords )
  end if

  if (present(north_pole))          self%north_pole          = north_pole
  if (present(null_island))         self%null_island         = null_island
  if (present(equatorial_latitude)) self%equatorial_latitude = equatorial_latitude
  if (present(coord_sys))           self%coord_sys           = coord_sys
  if (present(units_xy))            self%coord_units_xy(:)   = units_xy(:)

end subroutine set_coords


!-------------------------------------------------------------------------------
!> @brief  Assign connectivity related variables in the ugrid_2d file.
!>
!> @param[in] nodes_on_faces  Optional: Node ids connected to a given face
!> @param[in] edges_on_faces  Optional: Edge ids connected to a given face
!> @param[in] faces_on_faces  Optional: Face ids adjacent to a given face
!> @param[in] nodes_on_edges  Optional: Node ids connected to a given edge
!> @param[in] void_cell       Optional: The integer used to mark null connectivity.
!-------------------------------------------------------------------------------
subroutine set_connectivity( self,           &
                             nodes_on_faces, &
                             edges_on_faces, &
                             faces_on_faces, &
                             nodes_on_edges, &
                             void_cell )

  implicit none

  class (ugrid_2d_type), intent(inout) :: self

  integer(i_def), optional, intent(in) :: nodes_on_faces(:,:)
  integer(i_def), optional, intent(in) :: edges_on_faces(:,:)
  integer(i_def), optional, intent(in) :: faces_on_faces(:,:)
  integer(i_def), optional, intent(in) :: nodes_on_edges(:,:)
  integer(i_def), optional, intent(in) :: void_cell

  if ( present(nodes_on_faces) ) then
    if ( allocated( self%face_node_connectivity ) ) then
      deallocate( self%face_node_connectivity )
    end if
    allocate( self%face_node_connectivity, source=nodes_on_faces )
  end if

  if ( present(edges_on_faces) ) then
    if ( allocated( self%face_edge_connectivity ) ) then
      deallocate( self%face_edge_connectivity )
    end if
    allocate( self%face_edge_connectivity, source=edges_on_faces )
  end if

  if ( present(faces_on_faces) ) then
    if ( allocated( self%face_face_connectivity ) ) then
      deallocate( self%face_face_connectivity )
    end if
    allocate( self%face_face_connectivity, source=faces_on_faces )
  end if

  if ( present(nodes_on_edges) ) then
    if ( allocated( self%edge_node_connectivity ) ) then
      deallocate( self%edge_node_connectivity )
    end if
    allocate( self%edge_node_connectivity, source=nodes_on_edges )
  end if

  if ( present(void_cell) ) self%void_cell = void_cell

end subroutine set_connectivity


!-------------------------------------------------------------------------------
!> @brief       Assign data related to the local mesh partition in the
!>              ugrid_2d file.
!> @description Local mesh objects required additional data to describe the
!>              mesh partition they realte to. Partition data is not required
!>              for UGRID mesh topology.
!>
!> @param[in] node_cell_owner   Cell ID that owns a given node.
!> @param[in] edge_cell_owner   Cell ID that owns a given edge.
!> @param[in] num_inner         Number cells in each inner layer.
!> @param[in] num_halo          Number cells in each halo layer.
!> @param[in] last_inner_cell   Local index of last cell in each inner layer.
!> @param[in] last_halo_cell    Local index of last cell in each halo layer.
!> @param[in] cell_gid          Global index of cells in this local mesh object.
!> @param[in] node_on_cell_gid  Node on cell connectivity in Global IDs.
!> @param[in] edge_on_cell_gid  Edge on cell connectivity in Global IDs.
!-------------------------------------------------------------------------------
subroutine set_partition_data( self,               &
                               node_cell_owner,    &
                               edge_cell_owner,    &
                               num_inner,          &
                               num_halo,           &
                               last_inner_cell,    &
                               last_halo_cell,     &
                               cell_gid,           &
                               node_on_cell_gid,   &
                               edge_on_cell_gid  )

  implicit none

  class (ugrid_2d_type), intent(inout) :: self

  integer(i_def), intent(in) :: node_cell_owner(:)
  integer(i_def), intent(in) :: edge_cell_owner(:)

  integer(i_def), intent(in), allocatable :: num_inner(:)
  integer(i_def), intent(in), allocatable :: num_halo(:)
  integer(i_def), intent(in), allocatable :: last_inner_cell(:)
  integer(i_def), intent(in), allocatable :: last_halo_cell(:)

  integer(i_def), intent(in), allocatable :: cell_gid(:)
  integer(i_def), intent(in), allocatable :: node_on_cell_gid(:,:)
  integer(i_def), intent(in), allocatable :: edge_on_cell_gid(:,:)

  if (allocated( self%node_cell_owner ))  deallocate( self%node_cell_owner )
  if (allocated( self%edge_cell_owner ))  deallocate( self%edge_cell_owner )

  if (allocated( self%num_inner ))        deallocate( self%num_inner )
  if (allocated( self%num_halo ))         deallocate( self%num_halo )
  if (allocated( self%last_inner_cell ))  deallocate( self%last_inner_cell )
  if (allocated( self%last_halo_cell ))   deallocate( self%last_halo_cell )

  if (allocated( self%cell_gid ))         deallocate( self%cell_gid )
  if (allocated( self%node_on_cell_gid )) deallocate( self%node_on_cell_gid )
  if (allocated( self%edge_on_cell_gid )) deallocate( self%edge_on_cell_gid )


  allocate(self%node_cell_owner, source=node_cell_owner )
  allocate(self%edge_cell_owner, source=edge_cell_owner )

  if (allocated(cell_gid)) then
    allocate( self%cell_gid, source=cell_gid )
  end if

  if (allocated(node_on_cell_gid)) then
    allocate( self%node_on_cell_gid, source=node_on_cell_gid )
  end if

  if (allocated(edge_on_cell_gid)) then
    allocate( self%edge_on_cell_gid, source=edge_on_cell_gid )
  end if

  if (allocated(num_inner)) then
    allocate( self%num_inner, source=num_inner )
  end if

  if (allocated(num_halo)) then
    allocate( self%num_halo, source=num_halo )
  end if

  if (allocated(last_inner_cell)) then
    allocate( self%last_inner_cell, source=last_inner_cell )
  end if

  if (allocated(last_halo_cell)) then
    allocate( self%last_halo_cell, source=last_halo_cell )
  end if

end subroutine set_partition_data



!-------------------------------------------------------------------------------
!> @brief  Assign metadata variables in the ugrid_2d file.
!>
!> @param[in]   mesh_name          Optional: Name of the current mesh topology.
!> @param[in]   geometry           Optional: Domain geometry enumeration key.
!> @param[in]   topology           Optional: Domain topology enumeration key.
!> @param[in]   npanels            Optional: Number of panels used in mesh topology.
!> @param[in]   partition_of       Optional: Mesh name of partitioned global mesh.
!> @param[in]   ncells_global_mesh Optional: Number of cells in global mesh.
!> @param[in]   inner_depth        Optional: Number of inner halo layers.
!> @param[in]   halo_depth         Optional: Number of outer halo layers.
!> @param[in]   num_edge           Optional: Number of cells in the partition edge layer.
!> @param[in]   last_edge_cell     Optional: Local ID of last cell in partition edge layer.
!> @param[in]   num_ghost          Optional: Number of cells in the partition ghost layer.
!> @param[in]   last_ghost_cell    Optional: Local ID of last cell in partition ghost layer.
!> @param[in]   max_stencil_depth  Optional: Maximum stencil depth supported (Local meshes).
!> @param[in]   domain_extents     Optional: Principal coordinates that describe the
!>                                           domain shape.
!> @param[in]   rim_depth          Optional: Rim depth (in cells) for LBC meshes.
!> @param[in]   periodic_xy        Optional: Model domain periodicity in x/y-axes.
!> @param[in]   edge_cells_x       Optional: Number of cells on panel edge (x-axis).
!> @param[in]   edge_cells_y       Optional: Number of cells on panel edge (y-axis).
!> @param[in]   constructor_inputs Optional: Input arguments use to create this mesh.
!> @param[in]   nmaps              Optional: The number of intergrid maps from this mesh.
!> @param[in]   target_mesh_names  Optional: Names of target mesh topologies in this file
!>                                           which this mesh possesses cell-cell maps for.
!-------------------------------------------------------------------------------
subroutine set_metadata ( self,                &
                          mesh_name,           &
                          geometry,            &
                          topology,            &
                          npanels,             &
                          partition_of,        &
                          ncells_global_mesh,  &
                          inner_depth,         &
                          halo_depth,          &
                          num_edge,            &
                          last_edge_cell,      &
                          num_ghost,           &
                          last_ghost_cell,     &
                          max_stencil_depth,   &
                          domain_extents,      &
                          rim_depth,           &
                          periodic_xy,         &
                          edge_cells_x,        &
                          edge_cells_y,        &
                          constructor_inputs,  &
                          nmaps,               &
                          target_mesh_names )
  implicit none

  class(ugrid_2d_type), intent(inout) :: self

  character(str_def), optional, intent(in) :: mesh_name
  character(str_def), optional, intent(in) :: geometry
  character(str_def), optional, intent(in) :: topology
  character(str_def), optional, intent(in) :: partition_of

  character(str_longlong), optional, intent(in) :: constructor_inputs

  integer(i_def), optional, intent(in) :: npanels
  integer(i_def), optional, intent(in) :: ncells_global_mesh
  integer(i_def), optional, intent(in) :: inner_depth
  integer(i_def), optional, intent(in) :: halo_depth
  integer(i_def), optional, intent(in) :: num_edge
  integer(i_def), optional, intent(in) :: last_edge_cell
  integer(i_def), optional, intent(in) :: num_ghost
  integer(i_def), optional, intent(in) :: last_ghost_cell
  integer(i_def), optional, intent(in) :: max_stencil_depth
  integer(i_def), optional, intent(in) :: rim_depth
  integer(i_def), optional, intent(in) :: edge_cells_x
  integer(i_def), optional, intent(in) :: edge_cells_y
  real(r_def),    optional, intent(in) :: domain_extents(2,4)
  logical(l_def), optional, intent(in) :: periodic_xy(2)

  integer(i_def),     optional, intent(in) :: nmaps
  character(str_def), optional, intent(in) :: target_mesh_names(:)

  if (present(constructor_inputs)) self%constructor_inputs = constructor_inputs

  if (present(mesh_name))       self%mesh_name       = mesh_name
  if (present(geometry))        self%geometry        = geometry
  if (present(topology))        self%topology        = topology
  if (present(npanels))         self%npanels         = npanels
  if (present(partition_of))    self%partition_of    = partition_of
  if (present(inner_depth) )    self%inner_depth     = inner_depth
  if (present(halo_depth) )     self%halo_depth      = halo_depth
  if (present(num_edge))        self%num_edge        = num_edge
  if (present(last_edge_cell))  self%last_edge_cell  = last_edge_cell
  if (present(num_ghost))       self%num_ghost       = num_ghost
  if (present(last_ghost_cell)) self%last_ghost_cell = last_ghost_cell
  if (present(nmaps))           self%nmaps           = nmaps

  if (present(periodic_xy))  self%periodic_xy      = periodic_xy
  if (present(edge_cells_x)) self%edge_cells_xy(1) = edge_cells_x
  if (present(edge_cells_y)) self%edge_cells_xy(2) = edge_cells_y

  if (present(domain_extents)) then
    self%domain_extents(:,:) = domain_extents(:,:)
  end if

  if (present(rim_depth)) self%rim_depth = rim_depth

  if (present(max_stencil_depth))  self%max_stencil_depth = max_stencil_depth
  if (present(ncells_global_mesh)) self%num_faces_global  = ncells_global_mesh

  if (self%nmaps > 0 .and. present(target_mesh_names)) then
    if (allocated(self%target_mesh_names)) then
      deallocate(self%target_mesh_names)
    end if
    allocate(self%target_mesh_names, source=target_mesh_names)
  end if

  return
end subroutine set_metadata


!-------------------------------------------------------------------------------
!> @brief   Writes coordinates to a .dat file in the units they are held in
!>          within the UGRID file.
!> @details Produces a file with rough output of coordinates. Intended to be
!>          a temporary routine only: would be better implemented for the
!>          long-term as a plain-text ugrid file strategy. Hence the
!>          good-enough-for-now hardwired unit numbers and file names.
!>
!-------------------------------------------------------------------------------
subroutine write_coordinates(self)
  implicit none

  ! Arguments
  class(ugrid_2d_type), intent(in) :: self

  integer(i_def) :: inode

  open(56, file='nodes.dat')
  do inode = 1, self%num_nodes
    write(56,*) self%node_coordinates(:,inode)
  end do
  close(56)

  return
end subroutine write_coordinates

!-------------------------------------------------------------------------------
!> @brief Routine to destroy object.
!-------------------------------------------------------------------------------
subroutine clear(self)

  implicit none

  class (ugrid_2d_type), intent(inout) :: self

  if (allocated(self%node_coordinates))       deallocate( self%node_coordinates )
  if (allocated(self%face_coordinates))       deallocate( self%face_coordinates )

  if (allocated(self%face_node_connectivity)) deallocate( self%face_node_connectivity )
  if (allocated(self%edge_node_connectivity)) deallocate( self%edge_node_connectivity )
  if (allocated(self%face_edge_connectivity)) deallocate( self%face_edge_connectivity )
  if (allocated(self%face_face_connectivity)) deallocate( self%face_face_connectivity )

  if (allocated(self%target_mesh_names))      deallocate( self%target_mesh_names )
  if (allocated(self%target_edge_cells_x))    deallocate( self%target_edge_cells_x )
  if (allocated(self%target_edge_cells_y))    deallocate( self%target_edge_cells_y )

  if (allocated(self%file_handler))           deallocate( self%file_handler )

  self%mesh_name  = cmdi
  self%geometry   = cmdi
  self%topology   = cmdi
  self%coord_sys  = cmdi
  self%periodic_xy(:) = .false.

  self%max_stencil_depth  = imdi
  self%npanels            = imdi
  self%constructor_inputs = cmdi

  self%coord_units_xy = cmdi
  self%edge_cells_xy  = imdi

  self%num_cells = imdi
  self%num_nodes = imdi
  self%num_edges = imdi
  self%num_faces = imdi

  self%num_nodes_per_face = imdi
  self%num_nodes_per_edge = imdi
  self%num_edges_per_face = imdi
  self%max_num_faces_per_node = imdi

  self%nmaps               = 0
  self%rim_depth           = imdi
  self%domain_extents(:,:) = rmdi
  self%partition_of        = cmdi
  self%inner_depth         = imdi
  self%halo_depth          = imdi
  self%num_edge            = imdi
  self%last_edge_cell      = imdi
  self%num_ghost           = imdi
  self%last_ghost_cell     = imdi
  self%num_faces_global    = imdi

  self%target_global_mesh_maps => null()

end subroutine clear

!-------------------------------------------------------------------------------
!> @brief   Returns logical to determine it current contents represents a global
!>          or local mesh object.
!> @return  Logical  .True. if contents represent a logcal mesh.
!-------------------------------------------------------------------------------
function is_local(self) result(answer)

 implicit none

 class (ugrid_2d_type), intent(in) :: self

  logical(l_def) :: answer

  ! This simply tests on the whether of not this object is a
  ! "partition_of" another mesh. If the variable <partition_of>
  ! has not been set by 'set_metadata' it will be taken as a global
  ! mesh. The variable <partition_of> is reset every time the object
  ! is destroyed/cleared.
  if (self%partition_of == cmdi) then
    answer = .false.
  else
    answer = .true.
  end if

end function is_local


!-------------------------------------------------------------------------------
!> @brief Finalizer routine which should automatically call clear
!>        when object is out of scope.
!-------------------------------------------------------------------------------
subroutine ugrid_2d_destructor(self)

  implicit none

  type (ugrid_2d_type), intent(inout) :: self

  call self%clear()

end subroutine ugrid_2d_destructor

end module ugrid_2d_mod
