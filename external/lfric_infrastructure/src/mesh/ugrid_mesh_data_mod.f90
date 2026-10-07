










!-----------------------------------------------------------------------------
! (c) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used
!-----------------------------------------------------------------------------

!> @brief  Stores the information required to populate global mesh object.
!>
!> @details Stores the information that is required to populate a global mesh
!>          object. The data within the object can be read from a global
!>          mesh file.

module ugrid_mesh_data_mod

  use constants_mod, only: r_def, i_def, l_def, str_def, &
                           str_longlong, cmdi, imdi, rmdi
  use log_mod,       only: log_event, log_scratch_space, &
                           LOG_LEVEL_ERROR, LOG_LEVEL_TRACE

  implicit none

  private

  integer(i_def), parameter :: LOCAL_MESH_FLAG  = 100
  integer(i_def), parameter :: GLOBAL_MESH_FLAG = 101

  type, public :: ugrid_mesh_data_type
    !> Name of ugrid mesh topology.
    character(str_def) :: global_mesh_name

    integer(i_def)     :: mesh_extents
    logical(l_def)     :: is_local_mesh

    !> Domain geometry of global mesh.
    character(str_def) :: geometry
    !> Topology of mesh.
    character(str_def) :: topology
    !> Coordinate system used to specify node coordinates.
    character(str_def) :: coord_sys
    !> Units of any coordinate variables.
    character(str_def) :: coord_units_xy(2)  = cmdi
    !> Total number of nodes (vertices) in the full domain.
    integer(i_def) :: nnode
    !> total number of cells in full domain.
    integer(i_def) :: nface
    !> Total number of edges in the full domain.
    integer(i_def) :: nedge
    !> Number of nodes (vertices) on each cell.
    integer(i_def) :: num_nodes_per_face
    !> Number of nodes (vertices) on each edge.
    integer(i_def) :: num_nodes_per_edge
    !> Number of edges on each cell.
    integer(i_def) :: num_edges_per_face
    !> Maximum number of cells around a node (vertex).
    integer(i_def) :: max_num_faces_per_node
    !> Periodic in x/y-axes
    logical(l_def) :: periodic_xy(2)
    !> Number of other "target" global meshes that this mesh is mapped to
    integer(i_def) :: ntarget_meshes
    !> Names of other "target" global meshes that this mesh is mapped to.
    character(str_def),  allocatable :: target_global_mesh_names(:)
    !> Horizontal coords of nodes (vertices) in full domain.
    real(r_def), allocatable :: node_coords(:,:)
    !> Horizontal coords of face (cell) centres in full domain.
    real(r_def), allocatable :: face_coords(:,:)
    !> Full domain face to face (cell to cell) connectivities.
    integer(i_def), allocatable :: face_next_2d(:,:)
    !> Full domain nodes (vertices) on a cell.
    integer(i_def), allocatable :: node_on_face_2d(:,:)
    !> Full domain edges on a cell.
    integer(i_def), allocatable :: edge_on_face_2d(:,:)
    !> Full domain nodes on an edge.
    integer(i_def), allocatable :: node_on_edge_2d(:,:)

    !> Max. stencil depth (in cells) supported by local mesh.
    integer(i_def) :: max_stencil_depth = imdi
    !> Constructor inputs of global parent meshes,
    !> i.e. global cubedsphere / planar meshes.
    character(str_longlong) :: constructor_inputs = cmdi
    !> Number of groups of cells with uniform orientation
    !> (Global mesh)
    integer(i_def) :: npanels        = imdi
    !> Depth (in cells) of Global LBC mesh.
    integer(i_def) :: rim_depth      = imdi
    ! Real world lon-lat location of null_island.
    real(r_def)    :: null_island(2) = rmdi
    ! Real world lon-lat location of north pole.
    real(r_def)    :: north_pole(2)  = rmdi
    ! Latitude of equator following stretching.
    real(r_def)    :: equatorial_latitude = rmdi
    ! Domain extents (Global mesh).
    real(r_def)    :: domain_extents(2,4) = rmdi
    !> ID value to mark null cell-cell connectivity,
    integer(i_def) :: void_cell

    !> Depth (in cells) of partition inner region.
    integer(i_def) :: inner_depth

    !> Depth (in cells) of partition halo region.
    integer(i_def) :: halo_depth

    !> Number of cells in partition edge-layer.
    integer(i_def) :: num_edge

    !> Local ID of last in partition edge-layer.
    integer(i_def) :: last_edge_cell

    !> Number of cells in partition ghost-layer.
    integer(i_def) :: num_ghost

    !> Local ID of last in partition ghost-layer.
    integer(i_def) :: last_ghost_cell

    !> Number of faces (only if a global mesh).
    integer(i_def) :: num_faces_global

    !> Local cell ID which owns a given node(LID).
    integer(i_def), allocatable :: node_cell_owner(:)

    !> Local cell ID which owns a given edge(LID).
    integer(i_def), allocatable :: edge_cell_owner(:)

    !> Number of cells in each layer of the partition inner region.
    integer(i_def), allocatable :: num_inner(:)

    !> Number of cells in each layer of the partition halo region.
    integer(i_def), allocatable :: num_halo(:)

    !> Local ID of last cell in each layer of the
    !> partition inner region.
    integer(i_def), allocatable :: last_inner_cell(:)

    !> Local ID of last cell in each layer of the
    !> partition halo region.
    integer(i_def), allocatable :: last_halo_cell(:)

    !> Corresponding Global IDs of LIDS cells on local mesh.
    integer(i_def), allocatable :: cell_gid(:)

    !> Cell(LID)-Node(GIDs) connectivity of local mesh.
    integer(i_def), allocatable :: node_on_cell_gid(:,:)

    !> Cell(LID)-Edge(GIDs) connectivity of local mesh.
    integer(i_def), allocatable :: edge_on_cell_gid(:,:)

  contains
    procedure, public :: read_from_file
    procedure, public :: set_by_ugrid_2d
    procedure, public :: get_data
    procedure, public :: get_partition_data
    procedure, public :: is_local

    procedure, public :: clear
    final :: ugrid_mesh_data_destructor

  end type ugrid_mesh_data_type

contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Reads the information that is required to populate a global mesh
  !>        object out of a global mesh file.
  !>
  !> @param[in] filename         Filename for global 2D mesh(es) ugrid file.
  !> @param[in] global_mesh_name Name of ugrid mesh topology to create
  !>                             global mesh object from.
  !>
  subroutine read_from_file( self, filename, global_mesh_name )

    use ugrid_2d_mod,   only: ugrid_2d_type
    use ugrid_file_mod, only: ugrid_file_type
    use ncdf_quad_mod,  only: ncdf_quad_type

    implicit none

    class (ugrid_mesh_data_type), intent(inout) :: self
    character(*),                 intent(in)    :: filename
    character(*),                 intent(in)    :: global_mesh_name

    type(ugrid_2d_type) :: ugrid_2d

    class(ugrid_file_type), allocatable :: file_handler

    call self%clear()

    allocate( ncdf_quad_type :: file_handler )
    call ugrid_2d%set_file_handler( file_handler )
    call ugrid_2d%set_from_file_read( trim(global_mesh_name), trim(filename) )
    call self%clear()
    call self%set_by_ugrid_2d( ugrid_2d )

    if (ugrid_2d%is_local()) then
      self%mesh_extents = LOCAL_MESH_FLAG
    else
      self%mesh_extents = GLOBAL_MESH_FLAG
    end if

    if (allocated(file_handler)) deallocate( file_handler )

  end subroutine read_from_file


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Provides access to the data within the ugrid_mesh_data object - that
  !>        can be used to construct a global mesh object.
  !>
  !> @param[out] mesh_name Name of ugrid mesh topology.
  !> @param[out] geometry   Domain surface geometry.
  !> @param[out] topology   Domain topology.
  !> @param[out] coord_sys  Coordinate system used to position nodes.
  !> @param[out] npanels    Number of groups of uniform cell orientations on global mesh.
  !> @param[out] nnode      Total number of nodes in the full domain.
  !> @param[out] nedge      Total number of edges in the full domain.
  !> @param[out] nface      Total number of faces in full domain.
  !> @param[out] nnodes_per_face     Number of nodes on each face.
  !> @param[out] nnodes_per_edge     Number of nodes on each edge.
  !> @param[out] nedges_per_face     Number of edges on each cell.
  !> @param[out] max_faces_per_node  Maximum number of faces around a node.
  !> @param[out] periodic_xy         Domain periodicity in x/y-axes.
  !> @param[out] ntarget_meshes      Number of target global mesh name.
  !> @param[out] target_global_mesh_names   Target of global mesh name.
  !> @param[out] node_coords      Horizontal coords of vertices in full domain.
  !> @param[out] face_coords      Horizontal coords of cell centres in full domain.
  !> @param[out] coord_units_xy   Units for co-ordinates along x/y axes.
  !> @param[out] north_pole       Real world lon-lat location of mesh North Pole.
  !> @param[out] null_island      Real world lon-lat location of mesh Null Island.
  !> @param[out] equatorial_latitude Latitude of the equator of the mesh.
  !> @param[out] constructor_inputs  Configuration arguments for global mesh object constructor.
  !> @param[out] rim_depth        Global LBC mesh rim depth (in cells).
  !> @param[out] domain_extents   Principal coordinates that
  !>                              describe the domain shape.
  !> @param[out] void_cell        ID value to mark null cell-cell connectivity.
  !> @param[out] face_next_2d     Full domain cell to cell connectivities.
  !> @param[out] node_on_face_2d  Full domain vertices on a cell.
  !> @param[out] edge_on_face_2d  Full domain edges on a cell.
  !> @param[out] node_on_edge_2d  Full domain nodes on an edge.
  !> @param[out] max_stencil_depth  Max stencil depth supported by this mesh.
  !>
  subroutine get_data( self,      &
                       mesh_name, &
                       geometry,  &
                       topology,  &
                       coord_sys, &
                       npanels,   &
                       nnode,     &
                       nedge,     &
                       nface,     &
                       nnodes_per_face, &
                       nnodes_per_edge, &
                       nedges_per_face, &
                       max_faces_per_node, &
                       periodic_xy, &
                       ntarget_meshes, &
                       target_global_mesh_names, &
                       node_coords, &
                       face_coords, &
                       coord_units_xy, &
                       north_pole, &
                       null_island, &
                       equatorial_latitude, &
                       constructor_inputs, &
                       rim_depth, &
                       domain_extents, &
                       void_cell, &
                       face_next_2d, &
                       node_on_face_2d, &
                       edge_on_face_2d, &
                       node_on_edge_2d, &
                       max_stencil_depth )
    implicit none

    class (ugrid_mesh_data_type), intent(in) :: self
    character(str_def), intent(out) :: mesh_name
    character(str_def), intent(out) :: geometry
    character(str_def), intent(out) :: topology
    character(str_def), intent(out) :: coord_sys

    integer(i_def), intent(out) :: nnode
    integer(i_def), intent(out) :: nedge
    integer(i_def), intent(out) :: nface
    integer(i_def), intent(out) :: nnodes_per_face
    integer(i_def), intent(out) :: nnodes_per_edge
    integer(i_def), intent(out) :: nedges_per_face
    integer(i_def), intent(out) :: max_faces_per_node

    logical(l_def), intent(out) :: periodic_xy(2)
    integer(i_def), intent(out) :: ntarget_meshes

    character(str_def), intent(out),  allocatable :: target_global_mesh_names(:)

    real(r_def),    intent(out), allocatable :: node_coords(:,:)
    real(r_def),    intent(out), allocatable :: face_coords(:,:)
    integer(i_def), intent(out), allocatable :: face_next_2d(:,:)
    integer(i_def), intent(out), allocatable :: node_on_face_2d(:,:)
    integer(i_def), intent(out), allocatable :: edge_on_face_2d(:,:)

    integer(i_def), intent(out), allocatable, optional :: node_on_edge_2d(:,:)

    integer(i_def), intent(out) :: npanels
    integer(i_def), intent(out) :: rim_depth
    real(r_def),    intent(out) :: domain_extents(2,4)
    integer(i_def), intent(out) :: void_cell
    real(r_def),    intent(out) :: north_pole(2)
    real(r_def),    intent(out) :: null_island(2)
    real(r_def),    intent(out) :: equatorial_latitude

    character(str_def),      intent(out) :: coord_units_xy(2)
    character(str_longlong), intent(out) :: constructor_inputs

    ! Only valid if the ugrid file contains a local mesh
    integer(i_def), optional, intent(out) :: max_stencil_depth

    mesh_name = self%global_mesh_name
    geometry  = self%geometry
    topology  = self%topology
    coord_sys = self%coord_sys
    npanels   = self%npanels

    nnode = self%nnode
    nedge = self%nedge
    nface = self%nface
    nnodes_per_face = self%num_nodes_per_face
    nnodes_per_edge = self%num_nodes_per_edge
    nedges_per_face = self%num_edges_per_face
    max_faces_per_node = self%max_num_faces_per_node


    periodic_xy(:) = self%periodic_xy(:)
    ntarget_meshes = self%ntarget_meshes

    if ( ntarget_meshes > 0 ) then
      target_global_mesh_names = self%target_global_mesh_names
    end if

    node_coords = self%node_coords
    face_coords = self%face_coords

    void_cell = self%void_cell
    face_next_2d = self%face_next_2d
    node_on_face_2d = self%node_on_face_2d
    edge_on_face_2d = self%edge_on_face_2d

    if ( allocated(node_coords)  ) deallocate(node_coords)
    if ( allocated(face_coords)  ) deallocate(face_coords)
    if ( allocated(face_next_2d) ) deallocate(face_next_2d)
    if ( allocated(node_on_face_2d) ) deallocate(node_on_face_2d)
    if ( allocated(edge_on_face_2d) ) deallocate(edge_on_face_2d)

    allocate( node_coords,     source=self%node_coords  )
    allocate( face_coords,     source=self%face_coords  )
    allocate( face_next_2d,    source=self%face_next_2d )
    allocate( node_on_face_2d, source=self%node_on_face_2d )
    allocate( edge_on_face_2d, source=self%edge_on_face_2d )

    if (present(max_stencil_depth)) then
      max_stencil_depth = self%max_stencil_depth
    end if

    coord_units_xy      = self%coord_units_xy
    constructor_inputs  = self%constructor_inputs
    rim_depth           = self%rim_depth
    domain_extents(:,:) = self%domain_extents(:,:)
    north_pole(:)       = self%north_pole(:)
    null_island(:)      = self%null_island(:)
    equatorial_latitude = self%equatorial_latitude

    if ( present(node_on_edge_2d) ) then
      if ( allocated(node_on_edge_2d) ) deallocate(node_on_edge_2d)
      allocate( node_on_edge_2d, source=self%node_on_edge_2d )
    end if

  end subroutine get_data


  !-------------------------------------------------------------------------------
  !> @brief   Gets information related to local partitions.
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

    class (ugrid_mesh_data_type), intent(in) :: self

    integer(i_def), intent(out) :: max_stencil_depth
    integer(i_def), intent(out) :: inner_depth
    integer(i_def), intent(out) :: halo_depth
    integer(i_def), intent(out) :: num_edge
    integer(i_def), intent(out) :: num_ghost
    integer(i_def), intent(out) :: last_edge_cell
    integer(i_def), intent(out) :: last_ghost_cell
    integer(i_def), intent(out), allocatable :: node_cell_owner(:)
    integer(i_def), intent(out), allocatable :: edge_cell_owner(:)
    integer(i_def), intent(out), allocatable :: num_inner(:)
    integer(i_def), intent(out), allocatable :: num_halo(:)
    integer(i_def), intent(out), allocatable :: last_inner_cell(:)
    integer(i_def), intent(out), allocatable :: last_halo_cell(:)

    integer(i_def), intent(out) :: num_faces_global
    integer(i_def), intent(out), allocatable :: cell_gid(:)
    integer(i_def), intent(out), allocatable :: node_on_cell_gid(:,:)
    integer(i_def), intent(out), allocatable :: edge_on_cell_gid(:,:)


    if (.not. self%is_local()) then
      write(log_scratch_space,'(A)') &
        'Mesh '//trim(self%global_mesh_name)//' is a global mesh '//&
        'and does not contain partition data.'
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end if

    max_stencil_depth = self%max_stencil_depth
    inner_depth       = self%inner_depth
    halo_depth        = self%halo_depth
    num_edge          = self%num_edge
    num_ghost         = self%num_ghost
    last_edge_cell    = self%last_edge_cell
    last_ghost_cell   = self%last_ghost_cell
    last_ghost_cell   = self%last_ghost_cell
    num_faces_global  = self%num_faces_global

    if ( allocated( node_cell_owner ) )  deallocate( node_cell_owner )
    if ( allocated( edge_cell_owner ) )  deallocate( edge_cell_owner )
    if ( allocated( num_inner ) )        deallocate( num_inner )
    if ( allocated( num_halo ) )         deallocate( num_halo )
    if ( allocated( last_inner_cell ) )  deallocate( last_inner_cell )
    if ( allocated( last_halo_cell ) )   deallocate( last_halo_cell )
    if ( allocated( cell_gid ) )         deallocate( cell_gid )
    if ( allocated( node_on_cell_gid ) ) deallocate( node_on_cell_gid )
    if ( allocated( edge_on_cell_gid ) ) deallocate( edge_on_cell_gid )

    if (self%inner_depth > 0) then
      allocate( num_inner,       source=self%num_inner )
      allocate( last_inner_cell, source=self%last_inner_cell )
    end if

    if (self%halo_depth > 0) then
      allocate( num_halo,       source=self%num_halo )
      allocate( last_halo_cell, source=self%last_halo_cell )
    end if

    allocate( node_cell_owner,   source=self%node_cell_owner )
    allocate( edge_cell_owner,   source=self%edge_cell_owner )
    allocate( cell_gid,          source=self%cell_gid )
    allocate( node_on_cell_gid,  source=self%node_on_cell_gid )
    allocate( edge_on_cell_gid,  source=self%edge_on_cell_gid )

  end subroutine get_partition_data


  !-------------------------------------------------------------------------------
  !> @brief   Returns logical to determine it current contents represents a global
  !>          or local mesh object.
  !> @return  Logical  .True. if contents represent a logcal mesh.
  !-------------------------------------------------------------------------------
  function is_local(self) result(answer)

    implicit none

    class (ugrid_mesh_data_type), intent(in) :: self
    logical(l_def) :: answer

    answer = .false.
    select case (self%mesh_extents)
    case( LOCAL_MESH_FLAG )
      answer = .true.
    case( GLOBAL_MESH_FLAG )
      answer = .false.
    end select

  end function is_local


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Destroys a ugrid_mesh_data object when it is finished with.
  !>
  subroutine clear(self)

    implicit none

    class (ugrid_mesh_data_type), intent(inout) :: self

    if (allocated(self%node_coords))     deallocate( self%node_coords )
    if (allocated(self%face_coords))     deallocate( self%face_coords )
    if (allocated(self%face_next_2d))    deallocate( self%face_next_2d )
    if (allocated(self%node_on_face_2d)) deallocate( self%node_on_face_2d )
    if (allocated(self%edge_on_face_2d)) deallocate( self%edge_on_face_2d )
    if (allocated(self%node_on_edge_2d)) deallocate( self%node_on_edge_2d )

  end subroutine clear

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Destructor clears the allocated data in the object.
  !>
  subroutine ugrid_mesh_data_destructor(self)

    implicit none

    type (ugrid_mesh_data_type), intent(inout) :: self

    call self%clear()

    return

  end subroutine ugrid_mesh_data_destructor


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Populates ugrid_mesh_data_object from ugrid_2d object previously
  !>        set by file or ugrid_generator object.
  !>
  !> @param[in] ugrid_2d  Ugrid_2d object holding global mesh data.
  !>
  subroutine set_by_ugrid_2d( self, ugrid_2d )

    use ugrid_2d_mod, only: ugrid_2d_type

    implicit none

    class (ugrid_mesh_data_type), intent(inout) :: self
    class (ugrid_2d_type),        intent(in)    :: ugrid_2d

    call ugrid_2d%get_dimensions                                &
            ( num_nodes              = self%nnode,              &
              num_edges              = self%nedge,              &
              num_faces              = self%nface,              &
              num_nodes_per_face     = self%num_nodes_per_face, &
              num_edges_per_face     = self%num_edges_per_face, &
              num_nodes_per_edge     = self%num_nodes_per_edge, &
              max_num_faces_per_node = self%max_num_faces_per_node )

    call ugrid_2d%get_metadata                                &
            ( mesh_name           = self%global_mesh_name,    &
              geometry            = self%geometry,            &
              topology            = self%topology,            &
              coord_sys           = self%coord_sys,           &
              periodic_xy         = self%periodic_xy,         &
              max_stencil_depth   = self%max_stencil_depth,   &
              constructor_inputs  = self%constructor_inputs,  &
              north_pole          = self%north_pole,          &
              null_island         = self%null_island,         &
              equatorial_latitude = self%equatorial_latitude, &
              npanels             = self%npanels,             &
              domain_extents      = self%domain_extents,      &
              void_cell           = self%void_cell,           &
              rim_depth           = self%rim_depth,           &
              nmaps               = self%ntarget_meshes,      &
              target_mesh_names   = self%target_global_mesh_names )

    allocate( self%node_coords(2, self%nnode) )
    call ugrid_2d%get_node_coords(self%node_coords)

    allocate( self%face_coords(2, self%nface) )
    self%face_coords = ugrid_2d%get_face_coords()

    self%face_next_2d    = ugrid_2d%get_face_face_connectivity()
    self%node_on_face_2d = ugrid_2d%get_face_node_connectivity()
    self%edge_on_face_2d = ugrid_2d%get_face_edge_connectivity()
    self%node_on_edge_2d = ugrid_2d%get_edge_node_connectivity()

    call ugrid_2d%get_coord_units( self%coord_units_xy )

    if (ugrid_2d%is_local()) then

      self%mesh_extents = LOCAL_MESH_FLAG
      call ugrid_2d%get_partition_data(            &
                        self%max_stencil_depth,    &
                        self%inner_depth,          &
                        self%halo_depth,           &
                        self%num_inner,            &
                        self%num_edge,             &
                        self%num_halo,             &
                        self%num_ghost,            &
                        self%last_inner_cell,      &
                        self%last_edge_cell,       &
                        self%last_halo_cell,       &
                        self%last_ghost_cell,      &
                        self%node_cell_owner,      &
                        self%edge_cell_owner,      &
                        self%num_faces_global,     &
                        self%cell_gid,             &
                        self%node_on_cell_gid,     &
                        self%edge_on_cell_gid )

    else
      self%mesh_extents = GLOBAL_MESH_FLAG
    end if

    return
  end subroutine set_by_ugrid_2d

end module ugrid_mesh_data_mod
