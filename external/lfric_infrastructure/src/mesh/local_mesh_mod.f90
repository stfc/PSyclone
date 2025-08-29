!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Local (partitioned) 2D mesh object.
!>
!> This module provides details for a mesh_type which is generated using a
!> global mesh object and a partition object.
!>
module local_mesh_mod

  use constants_mod,   only: r_def, i_def, i_halo_index,   &
                             l_def, str_def, integer_type, &
                             str_longlong,                 &
                             imdi, rmdi, cmdi, emdi,       &
                             degrees_to_radians,           &
                             radians_to_degrees

  use global_mesh_map_collection_mod, only: global_mesh_map_collection_type
  use global_mesh_map_mod,            only: global_mesh_map_type
  use global_mesh_mod,                only: global_mesh_type
  use halo_comms_mod,                 only: halo_routing_type, &
                                            perform_halo_exchange
  use linked_list_data_mod,           only: linked_list_data_type
  use local_mesh_map_collection_mod,  only: local_mesh_map_collection_type
  use local_mesh_map_mod,             only: local_mesh_map_type
  use log_mod,                        only: log_event, log_scratch_space, &
                                            LOG_LEVEL_ERROR, LOG_LEVEL_TRACE, &
                                            LOG_LEVEL_INFO, LOG_LEVEL_DEBUG
  use partition_mod,                  only: partition_type

  implicit none

  private

  integer(i_def), parameter :: spherical_domain = 601
  integer(i_def), parameter :: planar_domain    = 602

  integer(i_def), parameter :: non_periodic_domain = 701
  integer(i_def), parameter :: channel_domain      = 702
  integer(i_def), parameter :: periodic_domain     = 703

  integer(i_def), parameter :: lon_lat_coords = 801
  integer(i_def), parameter :: xyz_coords     = 802

  type, extends(linked_list_data_type), public :: local_mesh_type

    private

  ! Variables referring to local mesh
  !====================================
  ! Tag name of mesh.
    character(str_def) :: mesh_name
  ! Domain surface geometry.
    integer(i_def)     :: geometry = emdi
  ! Domain boundaries topology.
    integer(i_def)     :: topology = emdi
  ! Co-ordinate system used to specify node locations.
    integer(i_def)     :: coord_sys = emdi
  ! Co-ordinate units along xy-axes.
    character(str_def) :: coord_units_xy(2) = cmdi
  ! Marker id for cells that do not exist for mesh.
    integer(i_def)     :: void_cell = 0_i_def
  ! North pole location [lon,lat] for ll/spherical mesh.
    real(r_def)        :: north_pole(2)  = rmdi
  ! Null island location [lon,lat] for ll/spherical mesh.
    real(r_def)        :: null_island(2) = rmdi
  ! Latitude of equator for cubed-sphere mesh.
    real(r_def)        :: equatorial_latitude = rmdi
  ! Number of vertices on each cell.
    integer(i_def)     :: nverts_per_cell
  ! Number of vertices on each edge.
    integer(i_def)     :: nverts_per_edge
  ! Number of edges on each cell.
    integer(i_def)     :: nedges_per_cell
  ! Number of unique vertices in the local mesh.
    integer(i_def)     :: n_unique_vertices
  ! Number of unique edges in the local mesh.
    integer(i_def)     :: n_unique_edges
  ! Horizontal coords of vertices in local domain.
    real(r_def),    allocatable :: vert_coords(:,:)
  ! Horizontal coords of the cell centres in local domain.
    real(r_def),    allocatable :: cell_coords(:,:)
  ! Local domain cell to cell lid connectivities.
    integer(i_def), allocatable :: cell_next(:,:)
  ! Local IDs of vertices connected to local 2D cell.
    integer(i_def), allocatable :: vert_on_cell(:,:)
  ! Local IDs of edges connected to local 2D cell.
    integer(i_def), allocatable :: edge_on_cell(:,:)
  ! Local IDs of vertices connected to local 2D edge.
    integer(i_def), allocatable :: vert_on_edge(:,:)


  ! Variables referring to local partition
  !========================================
  ! Number of cells in a 2D slice of the local partition.
  ! (not inc ghost cells).
    integer(i_def)     :: num_cells_in_layer
  ! A List of global cell IDs known to this local mesh, ordered with inner
  ! cells first followed by the edge cells and finally the halo cells ordered
  ! by depth of halo.
    integer(i_def), allocatable :: global_cell_id(:)
  ! Global IDs of vertices connected to local 2D cell.
    integer(i_def), allocatable :: vert_on_cell_gid(:,:)
  ! Global IDs of edges connected to local 2D cell.
    integer(i_def), allocatable :: edge_on_cell_gid(:,:)
  ! Global IDs of vertices connected to local 2D edge.
    integer(i_def), allocatable :: vert_on_edge_gid(:,:)
  ! Cell that "owns" each vertex.
    integer(i_def), allocatable :: vert_cell_owner(:)
  ! Cell that "owns" each edge.
    integer(i_def), allocatable :: edge_cell_owner(:)
  ! A list of the ranks that own all the cells known to this partition
  ! held in the order of cells in the <code>global_cell_id</code> array
    integer(i_def), allocatable :: cell_owner( : )
  ! The number of "inner" cells in the <code>global_cell_id</code> list -
  ! one entry for each depth of inner halo.
    integer(i_def), allocatable :: num_inner(:)
  ! The index of the last "inner" cell in the <code>global_cell_id</code> list -
  ! one entry for each depth of inner halo.
    integer(i_def), allocatable :: last_inner_cell(:)
  ! The depth to which inner halos are generated.
    integer(i_def)              :: inner_depth
  ! The number of "edge" cells in the <code>global_cell_id</code> list.
    integer(i_def)              :: num_edge
  ! The index of the last "edge" cell in the <code>global_cell_id</code> list.
    integer(i_def)              :: last_edge_cell
  ! The number of "halo" cells in the <code>global_cell_id</code> list -
  ! one entry for each depth of halo.
    integer(i_def), allocatable :: num_halo(:)
  ! The index of the last "halo" cell in the <code>global_cell_id</code> list -
  ! one entry for each depth of halo.
    integer(i_def), allocatable :: last_halo_cell(:)
  ! The depth to which halos are generated.
    integer(i_def)              :: halo_depth
  ! The number of "ghost" cells in the <code>global_cell_id</code> list.
    integer(i_def)              :: num_ghost
  ! The index of the last "ghost" cell in the <code>global_cell_id</code> list.
    integer(i_def)              :: last_ghost_cell
  ! Maximum stencil depth supported by this mesh partition.
    integer(i_def)              :: max_stencil_depth = imdi


  ! Variables referring to integrid maps
  !================================================================
  ! Number of intergrid mesh maps assigned to this mesh.
    integer(i_def) :: ntarget_meshes

  ! Mesh names or target meshes for intergrid maps.
    character(str_def), allocatable :: target_mesh_names(:)

  ! Collection of local mesh maps associated with this mesh.
    type(local_mesh_map_collection_type), allocatable :: local_mesh_maps


  ! Variables referring to the global mesh of which this
  ! local mesh is a member
  !================================================================
  ! Number of panels in the global mesh.
    integer(i_def) :: npanels

  ! Number of cells in the global mesh.
    integer(i_def) :: ncells_global_mesh

  ! Domain size of global mesh along x/y-axes
    real(r_def)    :: domain_extents(2,4) = rmdi

  ! Rim depth in cells (LBC meshes)
    integer(i_def) :: rim_depth = imdi

  ! Contructor arguments use to instantiate global mesh
    character(str_longlong)  :: constructor_inputs


  contains
    procedure, public :: initialise_full
    procedure, public :: initialise_lbc
    procedure, public :: initialise_from_ugrid_data
    procedure, public :: initialise_unit_test
    generic           :: initialise => initialise_full,            &
                                       initialise_lbc,             &
                                       initialise_from_ugrid_data, &
                                       initialise_unit_test
    procedure, public :: init_cell_owner
    procedure, public :: clear

    procedure, public :: get_mesh_name
    procedure, public :: get_nverts_per_cell
    procedure, public :: get_nverts_per_edge
    procedure, public :: get_nedges_per_cell
    procedure, public :: get_num_cells_in_layer
    procedure, public :: get_edge_gid_on_cell
    procedure, public :: get_vert_gid_on_cell
    procedure, public :: get_vert_coords
    procedure, public :: get_cell_coords
    procedure, public :: get_n_unique_vertices
    procedure, public :: get_n_unique_edges
    procedure, public :: get_edge_on_cell
    procedure, public :: get_vert_on_cell
    procedure, public :: get_vert_cell_owner
    procedure, public :: get_edge_cell_owner
    procedure, public :: get_cell_next
    procedure, public :: get_inner_depth
    procedure, public :: get_num_cells_inner
    procedure, public :: get_last_inner_cell
    procedure, public :: get_num_cells_edge
    procedure, public :: get_last_edge_cell
    procedure, public :: get_halo_depth
    procedure, public :: get_num_cells_halo
    procedure, public :: get_last_halo_cell
    procedure, public :: get_num_cells_ghost
    procedure, public :: get_cell_owner
    procedure, public :: get_num_panels_global_mesh
    procedure, public :: get_ncells_global_mesh
    procedure, public :: get_max_stencil_depth
    procedure, public :: get_gid_from_lid
    procedure, public :: get_lid_from_gid
    procedure, public :: add_local_mesh_map
    procedure, public :: get_local_mesh_map
    procedure, public :: get_target_mesh_names
    procedure, public :: get_mesh_maps
    procedure, public :: get_all_gid
    procedure, public :: as_ugrid_2d
    procedure, public :: get_void_cell
    procedure, public :: get_north_pole
    procedure, public :: get_null_island
    procedure, public :: get_equatorial_latitude

    procedure, public :: get_global_domain_extents

    procedure, public :: is_geometry_spherical
    procedure, public :: is_geometry_planar
    procedure, public :: is_topology_non_periodic
    procedure, public :: is_topology_channel
    procedure, public :: is_topology_periodic
    procedure, public :: is_coord_sys_xyz
    procedure, public :: is_coord_sys_ll

    final :: local_mesh_destructor
  end type local_mesh_type

  !> Counter variable to keep track of the next local mesh ID number to uniquely
  !! identify each different mesh
  integer(i_def), save :: local_mesh_id_counter = 0_i_def

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Initialises a local mesh object from global mesh and partition
  !>        objects.
  !>
  !> This local mesh object holds the connectivities which fully describe
  !> the 2D topology of the local (partitionied) mesh.
  !>
  !> @param [in] global_mesh   Global mesh object on which the partition is
  !>                           applied.
  !> @param [in] partition     Partition object to base the local mesh on.
  !> @param [in] name          Optional: Name to use for this mesh. If omitted,
  !>                                     the name of the global mesh it is based
  !>                                     on will be used.
  subroutine initialise_full ( self,        &
                               global_mesh, &
                               partition,   &
                               name )
    implicit none

    class(local_mesh_type), intent(out)          :: self
    type(global_mesh_type), intent(in), pointer  :: global_mesh
    type(partition_type),   intent(in)           :: partition
    character(str_def),     intent(in), optional :: name

    integer(i_def)                               :: depth
    integer(i_def)                               :: cell_lid
    integer(i_def)                               :: cell_gid
    integer(i_def)                               :: vertex
    integer(i_def)                               :: edge
    integer(i_def)                               :: counter
    integer(i_def), allocatable                  :: tmp_list(:)
    integer(i_def), allocatable                  :: vert_lid_gid_map(:)
    integer(i_def), allocatable                  :: edge_lid_gid_map(:)
    integer(i_def)                               :: vert_gid
    integer(i_def)                               :: edge_gid

    logical (l_def)                              :: new_unique_vertex
    logical (l_def)                              :: new_unique_edge

    integer(i_def)                               :: local_vertex

    integer(i_def), allocatable :: tmp_edge_lid_gid_map(:)
    integer(i_def), allocatable :: tmp_vert_on_edge_gid(:,:)

    ! Set name from either the given name - or the name of the global mesh.
    if (present(name)) then
      self%mesh_name = trim(name)
    else
      self%mesh_name = global_mesh%get_mesh_name()
    end if

    ! Inherit mesh properties from the parent global mesh.
    if (global_mesh%is_geometry_spherical()) then
      self%geometry = spherical_domain
    else if (global_mesh%is_geometry_planar()) then
      self%geometry = planar_domain
    end if

    if (global_mesh%is_topology_non_periodic()) then
      self%topology = non_periodic_domain
    else if (global_mesh%is_topology_channel()) then
      self%topology = channel_domain
    else if (global_mesh%is_topology_periodic()) then
      self%topology = periodic_domain
    end if

    if (global_mesh%is_coord_sys_xyz()) then
      self%coord_sys = xyz_coords
    else if (global_mesh%is_coord_sys_ll()) then
      self%coord_sys = lon_lat_coords
    end if

    self%domain_extents      = global_mesh%get_domain_extents()
    self%north_pole          = global_mesh%get_north_pole()
    self%null_island         = global_mesh%get_null_island()
    self%equatorial_latitude = global_mesh%get_equatorial_latitude()
    self%void_cell           = global_mesh%get_void_cell()

    ! Extract the info that makes up a local mesh from the
    ! global mesh and partition.
    local_mesh_id_counter = local_mesh_id_counter + 1
    call self%set_id( local_mesh_id_counter )

    self%constructor_inputs = global_mesh%get_constructor_inputs()
    self%nverts_per_cell    = global_mesh%get_nverts_per_cell()
    self%nverts_per_edge    = global_mesh%get_nverts_per_edge()
    self%nedges_per_cell    = global_mesh%get_nedges_per_cell()
    self%num_cells_in_layer = partition%get_num_cells_in_layer()
    self%max_stencil_depth  = partition%get_max_stencil_depth()

    allocate( self%global_cell_id(self%num_cells_in_layer) )
    self%global_cell_id = partition%get_global_cell_id()

    self%inner_depth = partition%get_inner_depth()
    allocate( self%num_inner(self%inner_depth) )
    allocate( self%last_inner_cell(self%inner_depth) )
    do depth = 1, self%inner_depth
      self%num_inner( depth ) = partition%get_num_cells_inner( depth )
      self%last_inner_cell( depth ) = partition%get_last_inner_cell( depth )
    end do

    self%num_edge = partition%get_num_cells_edge()
    self%last_edge_cell = partition%get_last_edge_cell()

    self%halo_depth = partition%get_halo_depth()
    allocate( self%num_halo(self%halo_depth) )
    allocate( self%last_halo_cell(self%halo_depth) )
    do depth = 1, self%halo_depth
      self%num_halo( depth ) = partition%get_num_cells_halo( depth )
      self%last_halo_cell( depth ) = partition%get_last_halo_cell( depth )
    end do

    self%num_ghost = partition%get_num_cells_ghost()
    self%last_ghost_cell = self%last_halo_cell(self%halo_depth) + self%num_ghost

    ! Allocate and initialise arrays to hold entity connectivities.

    ! Cells next to a cell.
    allocate( self%cell_next(self%nedges_per_cell, self%last_ghost_cell) )
    do cell_lid = 1, self%last_ghost_cell
      cell_gid = self%get_gid_from_lid(cell_lid)
      call global_mesh%get_cell_next( cell_gid, self%cell_next(:,cell_lid) )

      ! Convert the cell_next GIDs into LIDs.
      do edge = 1, self%nedges_per_cell
        if (self%get_lid_from_gid(self%cell_next(edge,cell_lid)) > 0) then
          self%cell_next(edge,cell_lid) = &
                 self%get_lid_from_gid(self%cell_next(edge,cell_lid))
        else
          ! If lid is zero (or -ve) cell next is outside domain - so set to zero.
          self%cell_next(edge,cell_lid) = global_mesh%get_void_cell() !0__i_def
        end if
      end do

    end do

    ! Vertices on a cell
    allocate( self%vert_on_cell_gid(self%nverts_per_cell, self%last_ghost_cell) )
    allocate( self%vert_on_cell(self%nverts_per_cell, self%last_ghost_cell) )
    allocate( tmp_list(self%last_ghost_cell*self%nverts_per_cell) )
    self%n_unique_vertices = 0_i_def
    do cell_lid = 1, self%last_ghost_cell
      cell_gid = self%get_gid_from_lid(cell_lid)
      call global_mesh%get_vert_on_cell( cell_gid, &
                                         self%vert_on_cell_gid(:,cell_lid) )
      do vertex = 1, self%nverts_per_cell
        new_unique_vertex = .true.
        do counter = 1, self%n_unique_vertices
          if (tmp_list(counter) == self%vert_on_cell_gid(vertex,cell_lid)) then
            new_unique_vertex = .false.
            self%vert_on_cell(vertex,cell_lid) = counter
            exit
          end if
        end do
        if ( new_unique_vertex ) then
          self%n_unique_vertices = self%n_unique_vertices + 1
          tmp_list(self%n_unique_vertices) = &
                             self%vert_on_cell_gid(vertex, cell_lid)
          self%vert_on_cell(vertex,cell_lid) = self%n_unique_vertices
        end if
      end do
    end do
    allocate( vert_lid_gid_map(self%n_unique_vertices) )
    vert_lid_gid_map(:) = tmp_list(1:self%n_unique_vertices)
    deallocate(tmp_list)

    ! Generate edge LIDS on local cells.
    !
    allocate( self%edge_on_cell_gid( self%nedges_per_cell,self%last_ghost_cell ) )
    allocate( self%edge_on_cell    ( self%nedges_per_cell,self%last_ghost_cell ) )

    allocate( tmp_edge_lid_gid_map(   self%last_ghost_cell*self%nedges_per_cell ) )
    allocate( tmp_vert_on_edge_gid( 2,self%last_ghost_cell*self%nedges_per_cell ) )

    self%n_unique_edges = 0_i_def

    do cell_lid = 1, self%last_ghost_cell

      cell_gid = self%get_gid_from_lid(cell_lid)
      call global_mesh%get_edge_on_cell( cell_gid, &
                                         self%edge_on_cell_gid(:,cell_lid) )
      do edge = 1, self%nedges_per_cell

        new_unique_edge = .true.

        ! Check to see if this edge lid has been registered already.
        do counter = 1, self%n_unique_edges
          if ( tmp_edge_lid_gid_map(counter) == self%edge_on_cell_gid(edge,cell_lid) ) then
            self%edge_on_cell(edge,cell_lid) = counter
            new_unique_edge = .false.
            exit
          end if
        end do

        if ( new_unique_edge ) then
          ! New unique edge identified.
          self%n_unique_edges = self%n_unique_edges + 1
          self%edge_on_cell(edge,cell_lid) = self%n_unique_edges

          ! Extract the GIDs of nodes attached to the edge (using LID).
          edge_gid = self%edge_on_cell_gid(edge, cell_lid)
          tmp_edge_lid_gid_map(self%n_unique_edges) = edge_gid
          tmp_vert_on_edge_gid(:,self%n_unique_edges) = &
                                              global_mesh%get_vert_on_edge(edge_gid)
        end if

      end do  ! edge
    end do  ! cell_lid

    allocate( edge_lid_gid_map, source=tmp_edge_lid_gid_map(1:self%n_unique_edges) )
    deallocate( tmp_edge_lid_gid_map )

    ! Populate verts on edges (LIDS).
    allocate( self%vert_on_edge_gid, source=tmp_vert_on_edge_gid(:,1:self%n_unique_edges) )
    deallocate(tmp_vert_on_edge_gid)

    allocate( self%vert_on_edge(2,self%n_unique_edges) )
    do edge=1, self%n_unique_edges
      do vertex=1,2
        do local_vertex=1, self%n_unique_vertices
          if ( self%vert_on_edge_gid(vertex, edge) == vert_lid_gid_map(local_vertex) ) then
            self%vert_on_edge(vertex, edge) = local_vertex
            exit
          end if
        end do
      end do
    end do

    ! Set cell ownership for each vertex.
    allocate( self%vert_cell_owner(self%n_unique_vertices) )
    do vertex=1,self%n_unique_vertices

      self%vert_cell_owner(vertex) = &
          self%get_lid_from_gid(     &
            global_mesh%get_vert_cell_owner(vert_lid_gid_map(vertex)) )

      if ( self%vert_cell_owner(vertex) == -1 ) then
        self%vert_cell_owner(vertex) = self%void_cell
      end if

    end do

    ! Set cell ownership for each edge.
    allocate( self%edge_cell_owner(self%n_unique_edges) )
    do edge=1,self%n_unique_edges

      self%edge_cell_owner(edge) = &
          self%get_lid_from_gid( &
            global_mesh%get_edge_cell_owner(edge_lid_gid_map(edge)) )
      if ( self%edge_cell_owner(edge) == -1 ) then
        self%edge_cell_owner(edge) = self%void_cell
      end if

    end do

    ! Get coords of vertices.
    allocate( self%vert_coords(3,self%n_unique_vertices) )
    self%vert_coords = 0.0_r_def
    do vertex = 1, self%n_unique_vertices
      vert_gid = vert_lid_gid_map(vertex)
      call global_mesh%get_vert_coords( vert_gid, self%vert_coords(:,vertex) )
    end do

    ! Get coords of cell centres.
    allocate( self%cell_coords(3,self%last_ghost_cell) )
    self%cell_coords = 0.0_r_def
    do cell_lid = 1, self%last_ghost_cell
      cell_gid = self%get_gid_from_lid(cell_lid)
      call global_mesh%get_cell_coords( cell_gid, self%cell_coords(:,cell_lid) )
    end do

    ! Internally held spherical coordinates should be in radians
    ! and only in degrees for output.
    self%coord_units_xy = global_mesh%get_coord_units()
    if ( self%is_coord_sys_ll() ) then
      if ( any(self%coord_units_xy /= 'radians') ) then
        ! Scale all coords so that they are in radians.
        self%coord_units_xy(:) = 'radians'
        self%vert_coords(:,:)  = self%vert_coords(:,:) * degrees_to_radians
        self%cell_coords(:,:)  = self%cell_coords(:,:) * degrees_to_radians
      end if
    end if

    self%npanels            = partition%get_num_panels_global_mesh()
    self%ncells_global_mesh = global_mesh%get_ncells()
    self%ntarget_meshes     = global_mesh%get_nmaps()
    if (self%ntarget_meshes > 0) then
      call global_mesh%get_target_mesh_names(self%target_mesh_names)
      if (.not. allocated(self%local_mesh_maps) ) then
        allocate( self%local_mesh_maps, source=local_mesh_map_collection_type() )
      end if
    end if

    deallocate( vert_lid_gid_map )
    deallocate( edge_lid_gid_map )

  end subroutine initialise_full


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Initialises a local mesh object for global LBC mesh cells
  !>        present in the partition relating to the corresponding
  !>        local_lam_mesh.
  !>
  !> @details This local mesh object holds the connectivities
  !>          which fully describes the 2D topology of the local LBC mesh.
  !>          This local LBC mesh is only valid for mapping to the
  !>          last edge cell of the corresponding local LAM partition,
  !>          i.e. there are no halo or ghost cells included.
  !>
  !> @param [in] global_lam_mesh  Global LAM mesh object.
  !> @param [in] global_lbc_mesh  Global LBC mesh object.
  !> @param [in] local_lam_mesh   Parent local mesh used to
  !>                              locate/spawn local LBC mesh from
  !> @param [in] name             Optional: Mesh tag name to use for this mesh.
  !>                              If omitted, the global mesh name it is based
  !>                              on will be used.
  !> @param [out] cell_map        Optional: Local LBC:LAM cell map.
  !>                              Provided so cell map can be added to local LBC
  !>                              mesh after initialisation.
  !> @param [out] edge_map        Optional: Local LBC:LAM edge map.
  !>                              Provided so edge map can be added to local LBC
  !>                              mesh after initialisation.
  !> @param [out] vert_map        Optional: Local LBC:LAM vert map.
  !>                              Provided so vert map can be added to local LBC
  !>                              mesh after initialisation.
  !>
  !> @todo Optional args, cell_map, edge_map, vert_map should really be part of
  !>       the initialisation routine, however due to a scoping issue the arrays
  !>       must be passed back and applied outside this routine. This should be
  !>       be addressed in future work. #3491 Should remove the need for this.
  subroutine initialise_lbc ( self,             &
                              global_lam_mesh,  &
                              global_lbc_mesh,  &
                              local_lam_mesh,   &
                              name,             &
                              cell_map,         &
                              edge_map,         &
                              vert_map )

  implicit none

  class(local_mesh_type), intent(out)          :: self

  type(global_mesh_type), intent(in), pointer  :: global_lam_mesh
  type(global_mesh_type), intent(in), pointer  :: global_lbc_mesh

  type(local_mesh_type),  intent(in), pointer  :: local_lam_mesh
  character(str_def),     intent(in), optional :: name

  integer(i_def), allocatable, intent(out), optional :: cell_map(:)
  integer(i_def), allocatable, intent(out), optional :: edge_map(:)
  integer(i_def), allocatable, intent(out), optional :: vert_map(:)

  ! Local vars
  integer(i_def), allocatable :: tmp_global_lbc_lam_map(:,:,:)
  integer(i_def), allocatable :: tmp_lam_lids(:)
  integer(i_def), allocatable :: tmp_lbc_gids(:)
  integer(i_def), allocatable :: tmp_cell_next(:)

  integer(i_def), allocatable :: unique_list(:)

  integer(i_def), allocatable :: edge_lbc_lid_gid_map(:)
  integer(i_def), allocatable :: vert_lbc_lid_gid_map(:)

  type(global_mesh_map_collection_type), pointer :: global_lbc_mesh_maps => null()
  type(global_mesh_map_type),            pointer :: global_lbc_mesh_map  => null()

  integer(i_def) :: global_lbc_ncells
  integer(i_def), allocatable :: global_lbc_lam_map(:)
  integer(i_def), allocatable :: local_lam_gids(:)

  integer(i_def), allocatable :: cell_lam_lbc_map(:)

  integer(i_def), allocatable :: cell_lbc_lam_map(:)
  integer(i_def), allocatable :: vert_lbc_lam_map(:)
  integer(i_def), allocatable :: edge_lbc_lam_map(:)

  integer(i_def) :: local_lam_last_edge_cell
  integer(i_def) :: n_layer_cells
  integer(i_def) :: n_ghost_cells
  real(r_def)    :: factor

  ! Counters
  integer(i_def) :: global_lbc_id
  integer(i_def) :: local_lbc_id
  integer(i_def) :: local_lbc_vert_id
  integer(i_def) :: local_lbc_edge_id
  integer(i_def) :: local_lbc_vert_gid
  integer(i_def) :: local_lbc_edge_gid
  integer(i_def) :: local_lam_id

  integer(i_def) :: i, vert, edge, vert_count, edge_count

  integer(i_def) :: lam_no_cell_next

  ! NO_CELL_NEXT parameter will indicate there is no
  ! cell at the given location. It also implies that
  ! the looping cell is on adjacent to the inner domain
  ! as a 0 (from the LAM cell next) will indicate the looping
  ! cell is on the outer edge of domain.
  integer(i_def) :: no_cell_next

  !     0000000000000000000000
  !     0+------------------+0
  !     0|  LBC Rim Cells   |0
  !     0| +--------------+ |0
  !     0| | no_cell_next | |0

  lam_no_cell_next = global_lam_mesh%get_void_cell()
  no_cell_next     = global_lbc_mesh%get_void_cell()

  ! All local meshes to have a unique id, note:
  ! This id is only valid for the program duration.
  local_mesh_id_counter = local_mesh_id_counter + 1
  call self%set_id( local_mesh_id_counter )

  !==============================================

  ! 1.1 Set the LBC mesh name
  !     Appends '-lbc' to the specified mesh name if
  !     present else use the name local mesh parent
  !     provided.
  if (present(name)) then
    self%mesh_name = trim(name)//'-lbc'
  else
    self%mesh_name = trim(local_lam_mesh%get_mesh_name())//'-lbc'
  end if

  ! 1.2 Set co-ordinate conversion factor.
  factor = 1.0_r_def
  if ( local_lam_mesh%is_coord_sys_ll() ) then
    ! For spherical co-ord system, internal
    ! units should be in radians.
    factor = radians_to_degrees
  end if

  ! 2.0 Initialise variables
  !==============================================
  !     These may be updated depending on the number of
  !     overlapping Local LBC/LAM cells.
  !
  self%last_ghost_cell    = imdi
  self%num_ghost          = 0_i_def
  self%ncells_global_mesh = global_lam_mesh%get_ncells()

  self%nedges_per_cell    = local_lam_mesh%get_nedges_per_cell()
  self%nverts_per_cell    = local_lam_mesh%get_nverts_per_cell()
  self%nverts_per_edge    = local_lam_mesh%get_nverts_per_edge()

  self%geometry  = local_lam_mesh%geometry
  self%coord_sys = local_lam_mesh%coord_sys
  self%topology  = non_periodic_domain

  self%npanels            = 1_i_def
  self%max_stencil_depth  = 0_i_def
  self%inner_depth        = 0_i_def
  self%halo_depth         = 0_i_def

  allocate(self%num_inner(self%inner_depth))
  self%num_inner = 0

  self%constructor_inputs = global_lam_mesh%get_constructor_inputs()
  self%rim_depth          = global_lbc_mesh%get_rim_depth()
  self%north_pole         = global_lbc_mesh%get_north_pole()
  self%null_island        = global_lbc_mesh%get_null_island()
  self%void_cell          = global_lbc_mesh%get_void_cell()

  self%ntarget_meshes     = 1_i_def
  self%coord_units_xy     = global_lbc_mesh%get_coord_units()
  self%domain_extents     = global_lam_mesh%get_domain_extents()

  allocate(self%target_mesh_names(1))
  self%target_mesh_names(1) = global_lam_mesh%get_mesh_name()

  if (allocated(self%local_mesh_maps)) deallocate(self%local_mesh_maps)
  allocate(self%local_mesh_maps)

  !===============================================================
  ! 3.0 Determine if this local partition contains any
  !     LBC cells. I.e. last_edge_cell > 0
  !===============================================================
  ! While looping here capture the local LBC maps:
  ! *) local LBC -> local  LAM map  (Local LBC id - Local mesh id)
  ! *) local LBC -> global LAM map  (Local LBC id - Global mesh id)
  !
  global_lbc_ncells    =  global_lbc_mesh%get_ncells()
  global_lbc_mesh_maps => global_lbc_mesh%get_mesh_maps()
  global_lbc_mesh_map  => global_lbc_mesh_maps%get_global_mesh_map(1,2)


  ! 3.1 Extract the global LBC-LAM cell map.
  !     LBC-LAM intergrid maps are always a 1:1 map, though are
  !     returned as a [1,1,ncells] array.
  call global_lbc_mesh_map%get_cell_map(tmp_global_lbc_lam_map)
  global_lbc_lam_map = reshape(tmp_global_lbc_lam_map, [global_lbc_ncells])
  if ( allocated(tmp_global_lbc_lam_map) ) deallocate(tmp_global_lbc_lam_map)


  ! 3.2 Determine the number of overlapping cells and maps.
  local_lam_gids           = local_lam_mesh%get_all_gid()
  local_lam_last_edge_cell = local_lam_mesh%get_last_edge_cell()
  n_layer_cells            = local_lam_mesh%get_num_cells_in_layer()
  n_ghost_cells            = local_lam_mesh%get_num_cells_ghost()

  allocate(tmp_lbc_gids(local_lam_last_edge_cell))
  allocate(tmp_lam_lids(local_lam_last_edge_cell))
  allocate(cell_lam_lbc_map(n_layer_cells + n_ghost_cells))

  cell_lam_lbc_map = self%void_cell

  local_lbc_id = 0_i_def

  do local_lam_id=1, local_lam_last_edge_cell
    ! Use global_LBC_LAM_map to check if the local LAM cell gid
    ! to see if it present on the LBC rim.
    if ( ANY( global_lbc_lam_map == local_lam_gids(local_lam_id)) ) then

      local_lbc_id = local_lbc_id + 1_i_def

      ! Loop over the global_lbc_lam_map to capture the
      ! global_lam IDs corresponding to the local LBC IDs
      ! on this partition.
      do global_lbc_id=1, global_lbc_ncells
        if ( local_lam_gids(local_lam_id) == global_lbc_lam_map(global_lbc_id) ) then
          tmp_lbc_gids(local_lbc_id) = global_lbc_id  ! Capture local lbc_id -> global lbc_id map
        end if
      end do

      tmp_lam_lids(local_lbc_id)     = local_lam_id   ! Capture local lbc_id -> local lam_id map
      cell_lam_lbc_map(local_lam_id) = local_lbc_id   ! Capture local lam_id -> local lbc_id map
    end if

  end do

  self%last_edge_cell     = local_lbc_id
  self%num_cells_in_layer = self%last_edge_cell
  self%last_ghost_cell    = self%last_edge_cell

  if ( self%last_edge_cell > 0_i_def ) then

    self%npanels            = 1_i_def
    self%ntarget_meshes     = 1_i_def

     if ( allocated(self%target_mesh_names) ) then
      deallocate(self%target_mesh_names)
    end if
    allocate(self%target_mesh_names(1))
    self%target_mesh_names(1) = global_lam_mesh%get_mesh_name()

    self%domain_extents = global_lam_mesh%get_domain_extents()
    self%north_pole     = global_lam_mesh%get_north_pole()
    self%null_island    = global_lam_mesh%get_null_island()
    self%coord_units_xy = global_lam_mesh%get_coord_units()

  else

    self%npanels            = 0_i_def
    self%ntarget_meshes     = 0_i_def
    self%n_unique_vertices  = 0_i_def
    self%n_unique_edges     = 0_i_def
    self%num_edge           = 0_i_def

    self%coord_units_xy(:)  = 'N/A'
    self%constructor_inputs = 'N/A'

    deallocate(tmp_lam_lids, tmp_lbc_gids)

    return

  end if

  allocate( cell_lbc_lam_map,    source=tmp_lam_lids(1:self%last_edge_cell) )
  allocate( self%global_cell_id, source=tmp_lbc_gids(1:self%last_edge_cell) )
  deallocate(tmp_lam_lids, tmp_lbc_gids)


  !==============================================================
  ! 4.0 Sets variable that are only relevant if there are
  !     LBC cells in this partition.
  !==============================================================

  ! 4.1 Generate Local LBC cell_next.
  !----------------------------------
  allocate( tmp_cell_next(self%nedges_per_cell) )
  allocate( self%cell_next(self%nedges_per_cell, self%last_edge_cell) )
  self%num_edge = 0_i_def

  do local_lbc_id=1, self%last_edge_cell

    local_lam_id = cell_lbc_lam_map(local_lbc_id)
    call local_lam_mesh%get_cell_next( local_lam_id, tmp_cell_next )

    do i=1, self%nedges_per_cell
      if (tmp_cell_next(i) == lam_no_cell_next) then
        tmp_cell_next(i) = no_cell_next
      else
        tmp_cell_next(i) = cell_lam_lbc_map( tmp_cell_next(i) )
      end if
    end do

    self%cell_next(:,local_lbc_id) = tmp_cell_next(:)

    ! 4.2 Capture number of cells on the edge of partition.
    !------------------------------------------------------
    ! If any cell has a NO_CELL_NEXT it is then an
    ! edge cell as the LBC local has no halos.
    if ( any(self%cell_next(:,local_lbc_id) == no_cell_next) ) then
      self%num_edge = self%num_edge + 1
    end if

  end do

  deallocate( tmp_cell_next )


  !======================================================================================
  ! 4.2 Generate Local LBC vert_on_cells.
  !======================================================================================
  allocate( self%vert_on_cell_gid( self%nverts_per_cell, &
                                   self%last_edge_cell ) )
  allocate( unique_list( self%last_edge_cell * self%nverts_per_cell ) )

  unique_list = -20
  vert_count  = 0_i_def

  do local_lbc_id=1, self%last_edge_cell
    call global_lbc_mesh%get_vert_on_cell( self%global_cell_id(local_lbc_id), &
                                           self%vert_on_cell_gid(:, local_lbc_id) )

    do vert=1, self%nverts_per_cell
      local_lbc_vert_gid = self%vert_on_cell_gid(vert, local_lbc_id)
      if ( .not. any(unique_list(:) == local_lbc_vert_gid) ) then
        vert_count = vert_count + 1_i_def
        unique_list(vert_count) = local_lbc_vert_gid
      end if
    end do
  end do

  self%n_unique_vertices = vert_count
  allocate( vert_lbc_lid_gid_map, &
            source=unique_list(1:self%n_unique_vertices) )
  deallocate(unique_list)

  ! Convert the LBC vert_on_cells GIDs to LIDs.
  allocate( self%vert_on_cell( self%nverts_per_cell, &
                               self%last_edge_cell ) )
  do local_lbc_id=1, self%last_edge_cell
    do vert=1, self%nverts_per_cell
      do i=1, self%n_unique_vertices
        if ( self%vert_on_cell_gid(vert, local_lbc_id) == vert_lbc_lid_gid_map(i) ) then
          self%vert_on_cell(vert, local_lbc_id) = i
          exit
        end if
      end do
    end do
  end do


  !======================================================================================
  ! 4.3 Generate Local LBC edge_on_cells.
  !======================================================================================
  allocate( self%edge_on_cell_gid( self%nedges_per_cell, &
                                   self%last_edge_cell ) )
  allocate( unique_list( self%last_edge_cell * self%nedges_per_cell ) )

  unique_list = -20_i_def
  edge_count  = 0_i_def

  do local_lbc_id=1, self%last_edge_cell
    call global_lbc_mesh%get_edge_on_cell( self%global_cell_id(local_lbc_id), &
                                           self%edge_on_cell_gid(:,local_lbc_id) )

    do edge=1, self%nedges_per_cell
      local_lbc_edge_gid = self%edge_on_cell_gid(edge, local_lbc_id)
      if ( .not. any(unique_list(:) == local_lbc_edge_gid) ) then
        edge_count = edge_count + 1_i_def
        unique_list(edge_count) = self%edge_on_cell_gid(edge,local_lbc_id)
      end if
    end do
  end do

  self%n_unique_edges = edge_count
  allocate( edge_lbc_lid_gid_map, &
            source=unique_list(1:self%n_unique_edges) )
  deallocate(unique_list)

  ! Convert the LBC edge_on_cells GIDs to LIDs.
  allocate( self%edge_on_cell( self%nedges_per_cell, &
                               self%last_edge_cell ) )
  do local_lbc_id=1, self%last_edge_cell
    do edge=1, self%nedges_per_cell
      local_lbc_edge_gid = self%edge_on_cell_gid(edge, local_lbc_id)
      do i=1, self%n_unique_edges
        if ( edge_lbc_lid_gid_map(i) == local_lbc_edge_gid ) then
          self%edge_on_cell(edge, local_lbc_id) = i
          exit
        end if
      end do
    end do
  end do


  !======================================================================================
  ! 4.4 Generate Local LBC vert on edges.
  !======================================================================================
  allocate( self%vert_on_edge_gid (2,self%n_unique_edges) )
  allocate( self%vert_on_edge     (2,self%n_unique_edges) )

  do local_lbc_edge_id=1, self%n_unique_edges

    self%vert_on_edge_gid(:, local_lbc_edge_id) = &
        global_lbc_mesh%get_vert_on_edge( edge_lbc_lid_gid_map(local_lbc_edge_id) )

    ! Convert the lbc vert_on_edge GIDs to LIDs
    do vert=1, 2
      local_lbc_vert_gid = self%vert_on_edge_gid(vert, local_lbc_edge_id)

      do i=1, self%n_unique_vertices
        if ( vert_lbc_lid_gid_map(i) == local_lbc_vert_gid ) then
          self%vert_on_edge(vert, local_lbc_edge_id) = i
          exit
        end if
      end do
    end do ! vert

  end do ! edge


  !======================================================================================
  ! 5.0 Assign coords to nodes and cell centres.
  !======================================================================================
  allocate( self%vert_coords(3, self%n_unique_vertices) )
  self%vert_coords = 0.0_r_def
  do local_lbc_vert_id=1, self%n_unique_vertices
    call global_lbc_mesh%get_vert_coords( vert_lbc_lid_gid_map(local_lbc_vert_id), &
                                          self%vert_coords(:,local_lbc_vert_id) )
  end do

  allocate( self%cell_coords(3, self%last_ghost_cell) )
  self%cell_coords = 0.0_r_def
  do local_lbc_id=1, self%last_ghost_cell
    global_lbc_id = self%get_gid_from_lid(local_lbc_id)
    call global_lbc_mesh%get_cell_coords( global_lbc_id, &
                                          self%cell_coords(:,local_lbc_id) )
  end do

  ! Internal coords of spherical units should be in radians
  ! and only output in degrees.
  if ( self%is_coord_sys_ll() ) then
    if ( any(self%coord_units_xy /= 'radians') ) then
      ! Scale all coords so that they are in radians.
      self%coord_units_xy(:) = 'radians'
      self%vert_coords(:,:)  = self%vert_coords(:,:) * degrees_to_radians
      self%cell_coords(:,:)  = self%cell_coords(:,:) * degrees_to_radians
    end if
  end if

  !======================================================================================
  ! 6.0 Generate cell ownership in terms of local IDs
  ! NOTE: -1 means that the global cell that owns this vert/edge cell is not on this
  !       local LBC mesh.
  !======================================================================================
  allocate( self%vert_cell_owner(self%n_unique_vertices) )
  do vert=1, self%n_unique_vertices

    self%vert_cell_owner(vert) = &
        self%get_lid_from_gid(   &
            global_lbc_mesh%get_vert_cell_owner( vert_lbc_lid_gid_map(vert) ) )

    if ( self%vert_cell_owner(vert) == -1 ) then
      self%vert_cell_owner(vert) = self%void_cell
    end if
  end do

  allocate( self%edge_cell_owner(self%n_unique_edges) )
  do edge=1, self%n_unique_edges
    self%edge_cell_owner(edge) = &
        self%get_lid_from_gid(   &
            global_lbc_mesh%get_edge_cell_owner( edge_lbc_lid_gid_map(edge) ) )

    if ( self%edge_cell_owner(edge) == -1 ) then
      self%edge_cell_owner(edge) = self%void_cell
    end if
  end do


  !======================================================================================
  ! 7.0 Extract cell/edge/vert LBC_LAM_maps (lid-lid) (if requested).
  !======================================================================================

  ! 7.1 Local LBC -> LAM cell map.
  !------------------------------------------
  if ( present(cell_map) ) then
    if (allocated(cell_map)) deallocate(cell_map)
    allocate( cell_map, source=cell_lbc_lam_map )
  end if

  ! 7.2 Local LBC -> LAM edge map.
  !------------------------------------------
  if ( present(edge_map) ) then

    allocate( edge_lbc_lam_map(self%n_unique_edges) )

    do local_lbc_id=1, self%last_edge_cell
      do edge=1, self%nedges_per_cell
        local_lbc_edge_id = self%edge_on_cell(edge,local_lbc_id)
        edge_lbc_lam_map( local_lbc_edge_id ) =    &
                local_lam_mesh%edge_on_cell( edge, &
                                             cell_lbc_lam_map(local_lbc_id) )
      end do
    end do

    if ( allocated(edge_map) ) deallocate(edge_map)
    allocate( edge_map, source=edge_lbc_lam_map )

    deallocate( edge_lbc_lam_map )

  end if

  ! 7.3 Local LBC -> LAM vertex map.
  !------------------------------------------
  if ( present(vert_map) ) then

    allocate( vert_lbc_lam_map(self%n_unique_vertices) )

    do local_lbc_id=1, self%last_edge_cell
      do vert=1, self%nverts_per_cell
        local_lbc_vert_id = self%vert_on_cell(vert,local_lbc_id)
        vert_lbc_lam_map( local_lbc_vert_id ) =    &
                local_lam_mesh%vert_on_cell( vert, &
                                             cell_lbc_lam_map(local_lbc_id) )
      end do
    end do

    if ( allocated(vert_map) ) deallocate(vert_map)
    allocate( vert_map, source=vert_lbc_lam_map )

  end if

  end subroutine initialise_lbc


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Initialises a local mesh object directly from data contained in a
  !>         <ugrid_mesh_data_type> object.
  !>
  !> @details Direct initialisation from a <ugrid_mesh_data_type> allows a local
  !>          mesh object to be populated directly from file read.
  !>
  !> @param [in] ugrid_mesh_data  <ugrid_mesh_data_type> which was populated
  !>                              directly from file read
  !>
  subroutine initialise_from_ugrid_data(self, ugrid_mesh_data)

    use ugrid_mesh_data_mod, only: ugrid_mesh_data_type

    implicit none

    class(local_mesh_type) :: self

    type(ugrid_mesh_data_type), intent(in) :: ugrid_mesh_data

    integer(i_def) :: nfaces            ! number of faces in this local mesh.
                                        ! should be same as last ghost cell
    integer(i_def) :: max_face_per_node ! Only needed for global meshes that
                                        ! are to be partitioned.

    logical(i_def) :: periodic_xy(2) = .false.

    character(str_def)       :: geometry_str
    character(str_def)       :: topology_str
    character(str_def)       :: coord_sys_str


    if (.not. ugrid_mesh_data%is_local()) then
      call log_event( 'Insufficient data to initialise local mesh', &
                      LOG_LEVEL_ERROR )
    end if

    local_mesh_id_counter = local_mesh_id_counter + 1
    call self%set_id( local_mesh_id_counter )

    ! Get ugrid data which describes the mesh.
    call ugrid_mesh_data%get_data(     &
             self%mesh_name,           &
             geometry_str,             &
             topology_str,             &
             coord_sys_str,            &
             self%npanels,             &
             self%n_unique_vertices,   &
             self%n_unique_edges,      &
             nfaces,                   &
             self%nverts_per_cell,     &
             self%nverts_per_edge,     &
             self%nedges_per_cell,     &
             max_face_per_node,        &
             periodic_xy,              &
             self%ntarget_meshes,      &
             self%target_mesh_names,   &
             self%vert_coords,         &
             self%cell_coords,         &
             self%coord_units_xy,      &
             self%north_pole,          &
             self%null_island,         &
             self%equatorial_latitude, &
             self%constructor_inputs,  &
             self%rim_depth,           &
             self%domain_extents,      &
             self%void_cell,           &
             self%cell_next,           &
             self%vert_on_cell,        &
             self%edge_on_cell )


    select case (trim(geometry_str))
    case ('spherical')
      self%geometry = spherical_domain
    case ('planar')
      self%geometry = planar_domain
    end select

    select case (trim(coord_sys_str))
    case ('ll')
      self%coord_sys=lon_lat_coords

      ! Ensure units are in radians
      if ( (trim(self%coord_units_xy(1)) == 'degrees_east') .and. &
           (trim(self%coord_units_xy(2)) == 'degrees_north') ) then

        self%vert_coords(:,:)    = degrees_to_radians * self%vert_coords(:,:)
        self%cell_coords(:,:)    = degrees_to_radians * self%cell_coords(:,:)
        self%domain_extents(:,:) = degrees_to_radians * self%domain_extents(:,:)
        self%north_pole(:)       = degrees_to_radians * self%north_pole(:)
        self%null_island(:)      = degrees_to_radians * self%null_island(:)
        self%equatorial_latitude = degrees_to_radians * self%equatorial_latitude

        self%coord_units_xy = 'radians'
      end if

    case ('xyz')
      self%coord_sys = xyz_coords
    end select

    select case (trim(topology_str))
    case ('channel')
      self%topology = channel_domain
    case ('non_periodic')
      self%topology = non_periodic_domain
    case ('periodic')
      self%topology = periodic_domain
    end select


    call ugrid_mesh_data%get_partition_data( &
             self%max_stencil_depth,         &
             self%inner_depth,               &
             self%halo_depth,                &
             self%num_inner,                 &
             self%num_edge,                  &
             self%num_halo,                  &
             self%num_ghost,                 &
             self%last_inner_cell,           &
             self%last_edge_cell,            &
             self%last_halo_cell,            &
             self%last_ghost_cell,           &
             self%vert_cell_owner,           &
             self%edge_cell_owner,           &
             self%ncells_global_mesh,        &
             self%global_cell_id,            &
             self%vert_on_cell_gid,          &
             self%edge_on_cell_gid )

  self%num_cells_in_layer = size(self%global_cell_id) - self%num_ghost

  if (.not. allocated(self%local_mesh_maps) ) then
    allocate ( self%local_mesh_maps, source=local_mesh_map_collection_type() )
  end if

  end subroutine initialise_from_ugrid_data



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Initialises a local mesh object for use in unit testing.
  !>
  !> @return New local_mesh_type object.
  !>
  subroutine initialise_unit_test ( self )

    implicit none

    class(local_mesh_type), intent(out) :: self

    ! Hard-coded simple local mesh for use in unit testing.
    ! Unfortunately it has to be consistent with the unit test version of
    ! the mesh object, which was put together before the current generators
    ! so doesn't follow the numbering scheme used currently by the generators.
    !
    ! This test 3x3 bi-periodic mesh uses the following numbering scheme:
    !
    !    1---2---2---5---5---8---1
    !    |       |       |       |
    !   16   7  17   8  18   9  16
    !    |       |       |       |
    !    8--12---7--14---9--15---8
    !    |       |       |       |
    !   10   4  11   5  13   6  10
    !    |       |       |       |
    !    4---4---3---7---6---9---4
    !    |       |       |       |
    !    1   1   3   2   6   3   1
    !    |       |       |       |
    !    1---2---2---5---5---8---1

    self%mesh_name = 'unit_test'

    self%geometry  = planar_domain
    self%topology  = periodic_domain
    self%coord_sys = xyz_coords
    self%void_cell = -9999_i_def

    local_mesh_id_counter = local_mesh_id_counter + 1
    call self%set_id( local_mesh_id_counter )

    self%nverts_per_cell    =  4_i_def
    self%nverts_per_edge    =  2_i_def
    self%nedges_per_cell    =  4_i_def
    self%ncells_global_mesh =  9_i_def
    self%num_cells_in_layer =  9_i_def
    self%n_unique_vertices  = 16_i_def
    self%n_unique_edges     = 24_i_def

    allocate( self%global_cell_id(9) )
    self%global_cell_id = [1,2,3,4,5,6,7,8,9]

    self%inner_depth = 1_i_def
    allocate( self%num_inner(self%inner_depth) )
    allocate( self%last_inner_cell(self%inner_depth) )

    self%num_inner(1)       = 9_i_def
    self%last_inner_cell(1) = 9_i_def

    self%num_edge       = 0_i_def
    self%last_edge_cell = 9_i_def

    self%halo_depth  = 3_i_def
    allocate( self%num_halo(self%halo_depth) )
    self%num_halo(1) = 0_i_def
    self%num_halo(2) = 0_i_def
    self%num_halo(3) = 0_i_def
    allocate( self%last_halo_cell(self%halo_depth) )
    self%last_halo_cell(1) = 9_i_def
    self%last_halo_cell(2) = 9_i_def
    self%last_halo_cell(3) = 9_i_def

    self%num_ghost = 0_i_def

    allocate( self%vert_cell_owner(self%n_unique_vertices) )
    self%vert_cell_owner(:) = [ 8_i_def, 9_i_def, 6_i_def, 1_i_def, &
                                5_i_def, 3_i_def, 2_i_def, 4_i_def, &
                                6_i_def, 4_i_def, 7_i_def, 9_i_def, &
                                7_i_def, 8_i_def, 9_i_def, 9 ]
    allocate( self%edge_cell_owner(self%n_unique_edges) )
    self%edge_cell_owner(:) = [ 1_i_def, 8_i_def, 6_i_def, 1_i_def, 2_i_def, 5_i_def, &
                                3_i_def, 2_i_def, 4_i_def, 3_i_def, 6_i_def, 4_i_def, &
                                4_i_def, 5_i_def, 7_i_def, 9_i_def, 6_i_def, 7_i_def, &
                                7_i_def, 8_i_def, 8_i_def, 9_i_def, 9_i_def, 9 ]

    allocate( self%cell_next(self%nedges_per_cell, self%num_cells_in_layer) )
    self%cell_next(:,1) = [3_i_def, 7_i_def, 2_i_def, 4_i_def]
    self%cell_next(:,2) = [1_i_def, 8_i_def, 3_i_def, 5_i_def]
    self%cell_next(:,3) = [2_i_def, 9_i_def, 1_i_def, 6_i_def]
    self%cell_next(:,4) = [6_i_def, 1_i_def, 5_i_def, 7_i_def]
    self%cell_next(:,5) = [4_i_def, 2_i_def, 6_i_def, 8_i_def]
    self%cell_next(:,6) = [5_i_def, 3_i_def, 4_i_def, 9_i_def]
    self%cell_next(:,7) = [9_i_def, 4_i_def, 8_i_def, 1_i_def]
    self%cell_next(:,8) = [7_i_def, 5_i_def, 9_i_def, 2_i_def]
    self%cell_next(:,9) = [8_i_def, 6_i_def, 7_i_def, 3_i_def]

    allocate( self%vert_on_cell(self%nverts_per_cell, self%num_cells_in_layer) )
    self%vert_on_cell(:, 1) = [ 1_i_def,  2_i_def,  3_i_def,  4_i_def]
    self%vert_on_cell(:, 2) = [ 2_i_def,  5_i_def,  6_i_def,  3_i_def]
    self%vert_on_cell(:, 3) = [ 5_i_def,  1_i_def,  4_i_def,  6_i_def]
    self%vert_on_cell(:, 4) = [ 4_i_def,  3_i_def,  7_i_def,  8_i_def]
    self%vert_on_cell(:, 5) = [ 3_i_def,  6_i_def,  9_i_def,  7_i_def]
    self%vert_on_cell(:, 6) = [ 6_i_def,  4_i_def,  8_i_def,  9_i_def]
    self%vert_on_cell(:, 7) = [ 8_i_def,  7_i_def,  2_i_def,  1_i_def]
    self%vert_on_cell(:, 8) = [ 7_i_def,  9_i_def,  5_i_def,  2_i_def]
    self%vert_on_cell(:, 9) = [ 9_i_def,  8_i_def,  1_i_def,  5_i_def]

    allocate( self%edge_on_cell(self%nedges_per_cell, self%num_cells_in_layer) )
    self%edge_on_cell(:, 1) = [ 1_i_def,  2_i_def,  3_i_def,  4_i_def]
    self%edge_on_cell(:, 2) = [ 3_i_def,  5_i_def,  6_i_def,  7_i_def]
    self%edge_on_cell(:, 3) = [ 6_i_def,  8_i_def,  1_i_def,  9_i_def]
    self%edge_on_cell(:, 4) = [10_i_def,  4_i_def, 11_i_def, 12_i_def]
    self%edge_on_cell(:, 5) = [11_i_def,  7_i_def, 13_i_def, 14_i_def]
    self%edge_on_cell(:, 6) = [13_i_def,  9_i_def, 10_i_def, 15_i_def]
    self%edge_on_cell(:, 7) = [16_i_def, 12_i_def, 17_i_def,  2_i_def]
    self%edge_on_cell(:, 8) = [17_i_def, 14_i_def, 18_i_def,  5_i_def]
    self%edge_on_cell(:, 9) = [18_i_def, 15_i_def, 16_i_def,  8_i_def]

    allocate( self%vert_on_cell_gid(self%nverts_per_cell, self%num_cells_in_layer) )
    self%vert_on_cell_gid(:, 1) = [1_i_def,  2_i_def,  3_i_def,  4_i_def]
    self%vert_on_cell_gid(:, 2) = [2_i_def,  5_i_def,  6_i_def,  3_i_def]
    self%vert_on_cell_gid(:, 3) = [5_i_def,  1_i_def,  4_i_def,  6_i_def]
    self%vert_on_cell_gid(:, 4) = [4_i_def,  3_i_def,  7_i_def,  8_i_def]
    self%vert_on_cell_gid(:, 5) = [3_i_def,  6_i_def,  9_i_def,  7_i_def]
    self%vert_on_cell_gid(:, 6) = [6_i_def,  4_i_def,  8_i_def,  9_i_def]
    self%vert_on_cell_gid(:, 7) = [8_i_def,  7_i_def,  2_i_def,  1_i_def]
    self%vert_on_cell_gid(:, 8) = [7_i_def,  9_i_def,  5_i_def,  2_i_def]
    self%vert_on_cell_gid(:, 9) = [9_i_def,  8_i_def,  1_i_def,  5_i_def]

    allocate( self%edge_on_cell_gid(self%nedges_per_cell, self%num_cells_in_layer) )
    self%edge_on_cell_gid(:, 1) = [ 1_i_def,  2_i_def,  3_i_def,  4_i_def]
    self%edge_on_cell_gid(:, 2) = [ 3_i_def,  5_i_def,  6_i_def,  7_i_def]
    self%edge_on_cell_gid(:, 3) = [ 6_i_def,  8_i_def,  1_i_def,  9_i_def]
    self%edge_on_cell_gid(:, 4) = [10_i_def,  4_i_def, 11_i_def, 12_i_def]
    self%edge_on_cell_gid(:, 5) = [11_i_def,  7_i_def, 13_i_def, 14_i_def]
    self%edge_on_cell_gid(:, 6) = [13_i_def,  9_i_def, 10_i_def, 15_i_def]
    self%edge_on_cell_gid(:, 7) = [16_i_def, 12_i_def, 17_i_def,  2_i_def]
    self%edge_on_cell_gid(:, 8) = [17_i_def, 14_i_def, 18_i_def,  5_i_def]
    self%edge_on_cell_gid(:, 9) = [18_i_def, 15_i_def, 16_i_def,  8_i_def]

    allocate( self%cell_owner(self%num_cells_in_layer+self%num_ghost) )
    self%cell_owner = 0_i_def

    self%npanels = 1_i_def

  end subroutine initialise_unit_test

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Calculate ownership of all cells known to this local mesh.
  !>
  !> An array representing all cells known to the local mesh has all the
  !> owned cells filled with the local rank ID (on every processor in
  !> parallel). When a halo swap is performed on this array, the array on
  !> every processor has its halo cells filled with their owner id.
  !>
  subroutine init_cell_owner(self)
    use lfric_mpi_mod,     only: global_mpi
    use fs_continuity_mod, only: W3

    implicit none
    class (local_mesh_type), intent(inout), target :: self
    type(halo_routing_type), pointer :: halo_routing => null()
    integer(i_halo_index), allocatable :: cell_id(:)
    integer(i_def), pointer :: cell_owner_ptr( : ) => null()
    integer(i_def) :: cell
    integer(i_def) :: i
    integer(i_def) :: last_owned_cell
    integer(i_def) :: total_inners
    integer(i_def) :: halo_start(1), halo_finish(1)
    integer(i_def) :: local_rank

    allocate( self%cell_owner(self%num_cells_in_layer+self%num_ghost) )
    self%cell_owner = 0_i_def

    ! Halo routines expect a 64-bit integer index - so convert global_cell_id.
    allocate( cell_id(size(self%global_cell_id)) )
    do i = 1, size(self%global_cell_id)
      cell_id(i) = self%global_cell_id(i)
    end do

    ! Work out the boundary between owned and halo cells.
    total_inners=0
    do i=1,self%inner_depth
      total_inners=total_inners+self%num_inner(i)
    end do
    last_owned_cell = total_inners+self%num_edge
    halo_start(1)  = last_owned_cell + 1_i_def
    halo_finish(1) = self%get_num_cells_in_layer()+self%get_num_cells_ghost()
    ! The above assumes there is a halo cell following the last owned cell.
    ! This might not be true (e.g. in a serial run), so fix the start/finish
    ! points when that happens.
    if(halo_start(1) > self%get_num_cells_in_layer())then
      halo_start(1)  = self%get_num_cells_in_layer()
      halo_finish(1) = self%get_num_cells_in_layer() - 1_i_def
    end if

    ! Set up a halo routing table for the cell owners data. Note this is
    ! cell data rather than field dof data, The routing table is defined by
    ! the first four arguments. The remaining arguments are used for finding the
    ! correct routing table in a collection and are therefore not required here
    ! as this is a one-use routing table. But to complete the argument list
    ! the closest description of this data is that it is lowest order,
    ! non-multidata, 32-bit integer, W3 data. As this is mesh data it's not
    !'on' a mesh as such - so just pass a mesh_id of zero.
    allocate(halo_routing)
    halo_routing = halo_routing_type( global_dof_id    = cell_id,         &
                                      last_owned_dof   = last_owned_cell, &
                                      halo_start       = halo_start,      &
                                      halo_finish      = halo_finish,     &
                                      mesh_id          = 0_i_def,         &
                                      element_order_h  = 0_i_def,         &
                                      element_order_v  = 0_i_def,         &
                                      lfric_fs         = W3,              &
                                      ndata            = 1_i_def,         &
                                      fortran_type     = integer_type,    &
                                      fortran_kind     = i_def,           &
                                      halo_depth       = self%halo_depth )

    deallocate(cell_id)

    ! Set ownership of all inner and edge cells to the local rank id
    ! - halo cells are unset.
    local_rank = global_mpi%get_comm_rank()
    do cell = 1,total_inners+self%num_edge
      self%cell_owner(cell)=local_rank
    end do

    ! Perform the halo swap to depth 1.
    cell_owner_ptr => self%cell_owner
    call perform_halo_exchange( cell_owner_ptr, &
                                halo_routing, &
                                1_i_def )

    call halo_routing%clear()
    deallocate(halo_routing)

  end subroutine init_cell_owner

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Fortran destructor, called when object goes out of scope.
  !>
  subroutine local_mesh_destructor(self)
    implicit none
    type (local_mesh_type), intent(inout) :: self

    call self%clear()

  end subroutine local_mesh_destructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Destroys a global_mesh object when it is finished with.
  !>
  subroutine clear(self)
    implicit none
    class (local_mesh_type), intent(inout) :: self

    if ( allocated( self%global_cell_id ) )  deallocate( self%global_cell_id )
    if ( allocated( self%cell_next ) )       deallocate( self%cell_next )
    if ( allocated( self%cell_owner ) )      deallocate( self%cell_owner )
    if ( allocated( self%num_inner ) )       deallocate( self%num_inner )
    if ( allocated( self%last_inner_cell ) ) deallocate( self%last_inner_cell )
    if ( allocated( self%num_halo ) )        deallocate( self%num_halo )
    if ( allocated( self%last_halo_cell ) )  deallocate( self%last_halo_cell )
    if ( allocated( self%vert_cell_owner) )  deallocate( self%vert_cell_owner)
    if ( allocated( self%edge_cell_owner) )  deallocate( self%edge_cell_owner)
    if ( allocated( self%local_mesh_maps ) ) deallocate( self%local_mesh_maps )

    return
  end subroutine clear

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Returns mesh tag name.
  !> @return mesh_name  Tag name of mesh.
  !>
  function get_mesh_name( self ) result ( mesh_name )
    implicit none
    class(local_mesh_type), intent(in) :: self
    character(str_def) :: mesh_name

    mesh_name = self%mesh_name

  end function get_mesh_name

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief   Returns the north pole location of mesh.
  !> @details A mesh may have been transformed before being written to file.
  !>          This returns the location of the north pole (in real world lon-lat)
  !>          referenced by co-ordinates in this mesh. Only valid for spherical
  !>          geometries with lon-lat coordinate system.
  !> @return  real(2), units (radians).
  !>
  function get_north_pole( self ) result( north_pole )

    implicit none

    class(local_mesh_type), intent(in) :: self
    real(r_def) :: north_pole(2)

    north_pole = self%north_pole

  end function get_north_pole


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief   Returns the null island location of mesh.
  !> @details A mesh may have been transformed before being written to file.
  !>          This returns the location of the null island (in real world lon-lat)
  !>          referenced by co-ordinates in this mesh. Only valid for spherical
  !>          geometries with lon-lat coordinate system.
  !> @return  null_island  [lon,lat] real-world coordinates in radians.
  !>
  function get_null_island( self ) result( null_island )

    implicit none

    class(local_mesh_type), intent(in) :: self
    real(r_def) :: null_island(2)

    null_island = self%null_island

  end function get_null_island


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief   Returns the latitude of the equator of the mesh.
  !> @details A mesh may have been transformed before being written to file.
  !!          This returns the latitde of the equator used for this mesh.
  !!          Only valid for cubed-sphere meshes.
  !> @return  equatorial_latitude   Latitude of equator of mesh
  !>
  function get_equatorial_latitude( self ) result( equatorial_latitude )

    implicit none

    class(local_mesh_type), intent(in) :: self
    real(r_def) :: equatorial_latitude

    equatorial_latitude = self%equatorial_latitude

  end function get_equatorial_latitude


  !---------------------------------------------------------------------------
  !> @brief  Queries if the local mesh domain geometry is spherical.
  !>
  !> @return answer .true. for a spherical domain surface.
  !>
  function is_geometry_spherical( self ) result ( answer )

    implicit none

    class(local_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%geometry == spherical_domain )

  end function is_geometry_spherical


  !---------------------------------------------------------------------------
  !> @brief  Queries if the local mesh domain geometry is a flat surface.
  !>
  !> @return answer .true. for a flat domain surface.
  !>
  function is_geometry_planar( self ) result ( answer )

    implicit none

    class(local_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%geometry == planar_domain )

  end function is_geometry_planar


  !---------------------------------------------------------------------------
  !> @brief  Queries if the local mesh topology specifies a domain where all the
  !>         boundaries are closed.
  !>
  !> @return answer .true. for a domain which is non-periodic.
  !>
  function is_topology_non_periodic( self ) result ( answer )

    implicit none

    class(local_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%topology == non_periodic_domain )

  end function is_topology_non_periodic


  !---------------------------------------------------------------------------
  !> @brief  Queries if the local mesh topology specifies a domain where there
  !>         is only one pair of entry/exit boundaries.
  !>
  !> @return answer .true. for a domain which forms a channel.
  !>
  function is_topology_channel( self ) result ( answer )

    implicit none

    class(local_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%topology == channel_domain )

  end function is_topology_channel


  !---------------------------------------------------------------------------
  !> @brief  Queries if the local mesh topology specifies a domain where all
  !>         domain boundaries are periodic.
  !>
  !> @return answer .true. for a domain which is periodic.
  !>
  function is_topology_periodic( self ) result ( answer )

    implicit none

    class(local_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%topology == periodic_domain )

  end function is_topology_periodic


  !---------------------------------------------------------------------------
  !> @brief  Queries if the local mesh nodes are specified using Cartesian
  !>         co-ordinates (x,y,z).
  !>
  !> @return answer .true. if nodes are specified in Cartesian co-ordinates.
  !>
  function is_coord_sys_xyz( self ) result ( answer )

    implicit none

    class(local_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%coord_sys == xyz_coords )

  end function is_coord_sys_xyz

  !---------------------------------------------------------------------------
  !> @brief  Queries if the local mesh nodes are specified using Spherical
  !>         co-ordinates (longitude, latitude).
  !>
  !> @return answer .true. if nodes are specified in spherical co-ordinates.
  !>
  function is_coord_sys_ll( self ) result ( answer )

    implicit none

    class(local_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%coord_sys == lon_lat_coords )

  end function is_coord_sys_ll


  !---------------------------------------------------------------------------
  !> @brief Gets the number of vertices per 2D-cell.
  !>
  !> @return Number of vertices per 2D-cell.
  !>
  function get_nverts_per_cell( self ) result (nverts_per_cell)

    implicit none
    class(local_mesh_type), intent(in) :: self
    integer(i_def)                     :: nverts_per_cell

    nverts_per_cell = self%nverts_per_cell

  end function get_nverts_per_cell

  !---------------------------------------------------------------------------
  !> @brief Gets the number of vertices per 2D-edge.
  !>
  !> @return Number of vertices per 2D-edge.
  !>
  function get_nverts_per_edge( self ) result (nverts_per_edge)

    implicit none
    class(local_mesh_type), intent(in) :: self
    integer(i_def)                     :: nverts_per_edge

    nverts_per_edge = self%nverts_per_edge

  end function get_nverts_per_edge

  !---------------------------------------------------------------------------
  !> @brief Gets the number of edges on each cell.
  !>
  !> @return Number of edges per 2D-cell.
  !>
  function get_nedges_per_cell( self ) result (nedges_per_cell)

    implicit none
    class(local_mesh_type), intent(in) :: self
    integer(i_def)                     :: nedges_per_cell

    nedges_per_cell = self%nedges_per_cell

  end function get_nedges_per_cell

  !---------------------------------------------------------------------------
  !> @brief Gets the total of all inner, edge and all halo cells in a 2d slice
  !>        on the local partition.
  !>
  !> @return The total number of the all inner, edge and halo cells on the
  !>         local partition.
  !>
  function get_num_cells_in_layer( self ) result ( num_cells )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def) :: num_cells

    num_cells = self%num_cells_in_layer

  end function get_num_cells_in_layer

  !> @details Returns the global edge id on the local cell.
  !> @param [in] iedge    The index of the edge whose global id is requested.
  !> @param [in] icell    The local id of the cell on which the edge is located.
  !> @return              Global id of the selected edge.
  !>
  function get_edge_gid_on_cell(self, iedge, icell) result (edge_gid)

    implicit none
    class(local_mesh_type), intent(in) :: self
    integer(i_def),         intent(in) :: iedge   ! Index of edge required.
    integer(i_def),         intent(in) :: icell   ! Local cell id.
    integer(i_def)                     :: edge_gid

    edge_gid = self%edge_on_cell_gid(iedge, icell)

  end function get_edge_gid_on_cell

  !> @details Returns the global vertex id on the local cell.
  !> @param [in] ivert    The index of the vertex whose global id is requested.
  !> @param [in] icell    The local id of the cell on which the vertex
  !>                      is located.
  !> @return              Global id of the selected vertex.
  !>
  function get_vert_gid_on_cell(self, ivert, icell) result (vert_gid)

    implicit none
    class(local_mesh_type), intent(in) :: self
    integer(i_def),         intent(in) :: ivert   ! Index of vertex required.
    integer(i_def),         intent(in) :: icell   ! Local cell id.
    integer(i_def)                     :: vert_gid

    vert_gid = self%vert_on_cell_gid(ivert, icell)

  end function get_vert_gid_on_cell

  !---------------------------------------------------------------------------
  !> @brief Gets vertex coordinates.
  !>
  !> @param[in]  vert_lid    Local ID of a vertex.
  !> @param[out] vert_coords Longitude and latitude of the specified vertex.
  !>
  subroutine get_vert_coords (self, vert_lid, vert_coords)

    implicit none
    class (local_mesh_type), intent(in)  :: self
    integer(i_def),          intent(in)  :: vert_lid
    real(r_def),             intent(out) :: vert_coords(:)

    vert_coords(1:2) = self%vert_coords(1:2,vert_lid)

  end subroutine get_vert_coords

  !---------------------------------------------------------------------------
  !> @brief Gets the coordinates.of the centre of a cell
  !>
  !> @param[in]  cell_lid    Local ID of a cell
  !> @param[out] cell_coords Longitude and latitude of the centre of the
  !>                         specified cell.
  !>
  subroutine get_cell_coords (self, cell_lid, cell_coords)

    implicit none
    class (local_mesh_type), intent(in)  :: self
    integer(i_def),          intent(in)  :: cell_lid
    real(r_def),             intent(out) :: cell_coords(:)

    cell_coords(1:2) = self%cell_coords(1:2,cell_lid)

  end subroutine get_cell_coords

  !---------------------------------------------------------------------------
  !> @brief Gets the number of unique vertices in the local domain.
  !>
  !> @return The number of unique vertices in the local domain.
  function get_n_unique_vertices( self ) result ( n_unique_vertices )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def) :: n_unique_vertices

    n_unique_vertices = self%n_unique_vertices

  end function get_n_unique_vertices

  !---------------------------------------------------------------------------
  !> @brief Gets the number of unique edges in the local domain.
  !>
  !> @return The number of unique edges in the local domain.
  function get_n_unique_edges( self ) result ( n_unique_edges )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def) :: n_unique_edges

    n_unique_edges = self%n_unique_edges

  end function get_n_unique_edges

  !> @details Returns the local edge id on the local cell.
  !> @param [in] iedge    The index of the edge whose local id is requested.
  !> @param [in] icell    The local id of the cell on which the edge is located.
  !> @return              Local id of the select edge.
  !>
  function get_edge_on_cell(self, iedge, icell) result (edge_lid)

    implicit none
    class(local_mesh_type), intent(in) :: self
    integer(i_def),         intent(in) :: iedge   ! Index of edge required
    integer(i_def),         intent(in) :: icell   ! Local cell id
    integer(i_def)                     :: edge_lid

    edge_lid = self%edge_on_cell(iedge, icell)

  end function get_edge_on_cell

  !> @details Returns the local vertex id on the local cell.
  !> @param [in] ivert    The index of the vertex whose local id is requested.
  !> @param [in] icell    The local id of the cell on which the vertex
  !>                      is located.
  !> @return              Local id of the selected vertex.
  !>
  function get_vert_on_cell(self, ivert, icell) result (vert_lid)

    implicit none
    class(local_mesh_type), intent(in) :: self
    integer(i_def),         intent(in) :: ivert   ! Index of vertex required
    integer(i_def),         intent(in) :: icell   ! Local cell id
    integer(i_def)                     :: vert_lid

    vert_lid = self%vert_on_cell(ivert, icell)

  end function get_vert_on_cell

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the cell that a vertex has been allocated to.
  !>
  !> @param[in] vert Local ID of the vertex.
  !>
  !> @return Local cell ID that the vertex has been allocated to.
  !>
  function get_vert_cell_owner ( self, vert ) result ( cell )

    implicit none
    class (local_mesh_type), intent(in)  :: self
    integer (i_def),         intent(in)  :: vert
    integer (i_def)                      :: cell

    cell = self%vert_cell_owner( vert )

  end function get_vert_cell_owner

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the cell that an edge has been allocated to.
  !>
  !> @param[in] edge Local ID of the edge.
  !>
  !> @return Local cell ID that the edge has been allocated to.
  !>
  function get_edge_cell_owner ( self, edge ) result ( cell )

    implicit none
    class (local_mesh_type), intent(in)  :: self
    integer (i_def),         intent(in)  :: edge
    integer (i_def)                      :: cell

    cell = self%edge_cell_owner( edge )

  end function get_edge_cell_owner

  !---------------------------------------------------------------------------
  !> @brief Gets the local IDs of the cells adjacent to a cell.
  !>
  !> @param[in]  cell_lid  Local ID of a cell.
  !> @param[out] cell_next Local IDs of the cells adjacent to cell cell_lid.
  !>
  subroutine get_cell_next (self, cell_lid, cell_next)

    implicit none
    class(local_mesh_type), intent(in)  :: self
    integer(i_def),         intent(in)  :: cell_lid
    integer(i_def),         intent(out) :: cell_next(:)

    cell_next(:) = self%cell_next(:,cell_lid)

  end subroutine get_cell_next

  !---------------------------------------------------------------------------
  !> @brief Gets the maximum depth of the inner halos.
  !>
  !> @return The maximum depth of the inner halos.
  !>
  function get_inner_depth( self ) result ( inner_depth )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def) :: inner_depth

    inner_depth = self%inner_depth

  end function get_inner_depth

  !---------------------------------------------------------------------------
  !> @brief Gets number of cells in an inner halo.
  !>
  !> @details Returns the total number of inner halo cells in a particular
  !>          depth of inner halo in a 2D slice on the local partition.
  !>
  !> @param[in] depth The level of inner halo being queried.
  !>
  !> @return Total number of inner halo cells at a particular depth on
  !>         the local partition. If depth is greater than the actual depth
  !>         of the halo then 0 will be returned.
  !>
  function get_num_cells_inner( self, depth ) result ( inner_cells )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def)             :: inner_cells

    if( depth > self%inner_depth .or. depth < 1 )then
      inner_cells = 0
    else
      inner_cells = self%num_inner(depth)
    end if

  end function get_num_cells_inner

  !---------------------------------------------------------------------------
  !> @brief Gets the index of the last cell in an inner halo.
  !>
  !> @details Returns the index of the last cell in a particular depth of
  !>          inner halo in a 2D slice on the local partition.
  !>
  !> @param[in] depth The level of inner halo being queried.
  !>
  !> @return Index of the last cell in the particular depth of inner halo on
  !>         the local partition.
  !>
  function get_last_inner_cell( self, depth ) result ( last_inner_cell )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def)             :: last_inner_cell

    if( depth > self%inner_depth .or. depth < 0)then
      last_inner_cell = 0
    else if( depth == 0 )then
      ! The zeroth depth inner halo has no size, so its last cell is in the
      ! same place as the inner halo before it in memory: inner(1).
      last_inner_cell = self%last_inner_cell(1)
    else
      last_inner_cell = self%last_inner_cell(depth)
    end if

  end function get_last_inner_cell

  !---------------------------------------------------------------------------
  !> @brief Gets the total number of edge cells in a 2D slice on the local
  !>        partition.
  !>
  !> @return Total number of "edge" cells on the local partition.
  !>
  function get_num_cells_edge( self ) result ( edge_cells )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def) :: edge_cells

    edge_cells = self%num_edge

  end function get_num_cells_edge

  !---------------------------------------------------------------------------
  !> @brief Gets the index of the last edge cell in a 2D slice on the local
  !>        partition.
  !>
  !> @return Index of the last of "edge" cell on the local partition.
  !>
  function get_last_edge_cell( self ) result ( last_edge_cell )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def) :: last_edge_cell

    last_edge_cell = self%last_edge_cell

  end function get_last_edge_cell

  !---------------------------------------------------------------------------
  !> @brief Gets the maximum depth of the halo of this partition.
  !>
  !> @return Maximum depth of halo cells.
  !>
  function get_halo_depth( self ) result ( halo_depth )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def) :: halo_depth

    halo_depth = self%halo_depth

  end function get_halo_depth


  !---------------------------------------------------------------------------
  !> @brief Gets number of cells in a halo.
  !>
  !> @details Returns the total number of halo cells in a particular depth
  !>          of halo in a 2D slice on the local partition.
  !>
  !> @param[in] depth The depth of the halo being queried.
  !>
  !> @return Total number of halo cells of the particular depth on the local
  !>         partition.
  !>
  function get_num_cells_halo( self, depth ) result ( halo_cells )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def)             :: halo_cells

    if( depth > self%halo_depth .or. depth < 1)then
      halo_cells = 0
    else
      halo_cells = self%num_halo(depth)
    end if

  end function get_num_cells_halo

  !---------------------------------------------------------------------------
  !> @brief Gets the index of the last cell in a halo.
  !>
  !> @details Returns the index of the last cell in a particular depth of halo
  !>          in a 2D slice on the local partition.
  !>
  !> @param[in] depth The depth of the halo being queried.
  !>
  !> @return Index of the last cell in the particular depth of halo on the
  !>         local partition.
  !>
  function get_last_halo_cell( self, depth ) result ( last_halo_cell )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def)             :: last_halo_cell

    if ( depth > self%halo_depth .or. depth < 0 ) then
      last_halo_cell = 0
    else if (depth == 0) then
      ! The zeroth depth halo has no size, so its last cell is in the
      ! same place as the last edge cell.
      last_halo_cell = self%get_last_edge_cell()
    else
      last_halo_cell = self%last_halo_cell(depth)
    end if

  end function get_last_halo_cell

  !---------------------------------------------------------------------------
  !> @brief Gets the total number of ghost cells in a slice around the local
  !>        partition.
  !>
  !> @return Total number of ghost cells around the local partition.
  !>
  function get_num_cells_ghost( self ) result ( ghost_cells )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def) :: ghost_cells

    ghost_cells = self%num_ghost

  end function get_num_cells_ghost

  !---------------------------------------------------------------------------
  !> @brief Gets the owner of a cell on the local partition.
  !>
  !> @param[in] cell_number Local ID of of the cell being queried.
  !>
  !> @return Owner of the given cell.
  !>
  function get_cell_owner( self, cell_number ) result ( cell_owner )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: cell_number

    integer(i_def) :: cell_owner

    cell_owner = self%void_cell
    if ( allocated( self%cell_owner ) )then
      cell_owner=self%cell_owner(cell_number)
    else
      call log_event( "Cell ownership not initialised", LOG_LEVEL_ERROR )
    end if

  end function get_cell_owner

  !---------------------------------------------------------------------------
  !> @brief Gets the number of panels in the global mesh.
  !>
  !> @return Number of panels.
  !>
  function get_num_panels_global_mesh ( self ) result ( number_of_panels )

    implicit none

    class(local_mesh_type), intent(in) :: self
    integer(i_def) :: number_of_panels

    number_of_panels = self%npanels

  end function get_num_panels_global_mesh


  !---------------------------------------------------------------------------
  !> @brief Gets the maximum stencil depth supported by this partition.
  !>
  !> @return Maximum supported stencil depth.
  !>
  function get_max_stencil_depth( self ) result ( max_stencil_depth )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def) :: max_stencil_depth

    max_stencil_depth = self%max_stencil_depth

  end function get_max_stencil_depth

  !---------------------------------------------------------------------------
  !> @brief Gets the number cells in the global mesh.
  !>
  !> @return Number of cells in the global mesh.
  !>
  function get_ncells_global_mesh ( self ) result ( ncells_global_mesh )

    implicit none

    class(local_mesh_type), intent(in) :: self
    integer(i_def) :: ncells_global_mesh

    ncells_global_mesh = self%ncells_global_mesh

  end function get_ncells_global_mesh

  !---------------------------------------------------------------------------
  !> @brief Gets the global index of the cell that corresponds to the given
  !>        local index on the local partition.
  !>
  !> @param[in] lid ID of a cell in local index space.
  !>
  !> @return ID of a cell in global index space.
  !>
  function get_gid_from_lid( self, lid ) result ( gid )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: lid           ! local index
    integer(i_def)             :: gid           ! global index

    gid = self%global_cell_id(lid)

  end function get_gid_from_lid

  !---------------------------------------------------------------------------
  !> @brief Gets the whole LID-GID map for cells on this partition.
  !>
  !> @return gids Integer array of LID-GID map, index is local cell ID, value is
  !>              corresponding global ID of the same cell.
  !>
  function get_all_gid( self) result ( gids )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def), allocatable :: gids(:)

    if (allocated(gids)) deallocate(gids)
    allocate( gids, source=self%global_cell_id )

  end function get_all_gid

  !---------------------------------------------------------------------------
  !> @brief Gets the local index of the cell on the local partition that
  !>        corresponds to the given global index.
  !>
  !> @param[in] gid Global index to search for on the local partition.
  !>
  !> @return Local index that corresponds to the given global index or -1 if
  !>         the cell with the given global index is not present of the local
  !>         partition.
  !>
  function get_lid_from_gid( self, gid ) result ( lid )
  !
  ! Performs a search through the global cell lookup table looking for the
  ! required global index.
  !
  ! The partitioned_cells array holds global indices in various groups:
  ! the inner halos, then the edge cells, the halo cells and finally
  ! the ghost cells. The cells are numerically ordered within the different
  ! groups so a binary search can be used, but not between groups, so need to do
  ! separate binary searches through the inner, edge, halo and ghost cells and
  ! exit if a match is found.
  !
    implicit none

    class(local_mesh_type), intent(in) :: self
    integer(i_def),         intent(in) :: gid  ! global index

    integer(i_def) :: lid                      ! local index (returned)

    integer(i_def) :: num_searched             ! Num cells alreadt searched
    integer(i_def) :: depth                    ! loop counter over halo depths
    integer(i_def) :: start_search             ! start point for a search
    integer(i_def) :: end_search               ! end point for a search

    ! Set the default return code
    lid = -1

    ! If the supplied gid is not valid just return
    if (gid < 1) return

    num_searched = 0
    ! Search though the inner halo cells - looking for the gid
    end_search = 0
    do depth = self%inner_depth, 1, -1
      start_search = end_search + 1
      end_search = start_search + self%num_inner(depth) - 1

      lid = binary_search( self%global_cell_id( start_search:end_search ), gid )
      if (lid /= -1) then
        lid = lid + num_searched
        return
      end if
      num_searched = num_searched + self%num_inner(depth)
    end do

    ! Search though edge cells - looking for the gid.
    start_search = end_search + 1
    end_search = start_search + self%num_edge - 1

    lid = binary_search( self%global_cell_id( start_search:end_search ), gid )
    if (lid /= -1) then
      lid = lid + num_searched
      return
    end if
    num_searched = num_searched + self%num_edge

    ! Search though halo cells - looking for the gid.
    do depth = 1,self%halo_depth + 1
      start_search = end_search + 1
      if (depth <= self%halo_depth) then
        end_search = start_search + self%num_halo(depth) - 1
      else
        end_search = start_search + self%num_ghost - 1
      end if

      lid = binary_search( self%global_cell_id( start_search:end_search ), gid )
      if (lid /= -1) then
        lid = lid + num_searched
        return
      end if
      if (depth <= self%halo_depth) then
        num_searched = num_searched + self%num_halo(depth)
      end if
    end do

    ! No lid has been found in either the inner, edge, halo or ghost cells
    ! on this partition, so return with lid=-1
    return

  end function get_lid_from_gid

  !==============================================================================
  !> @brief     Adds a local_mesh_map object to the mesh map collection
  !.            held in this local mesh.
  !> @param[in] target_local_mesh_id  ID of the target local mesh
  !>                                  object for this local mesh map object.
  !> @param[in] map
  !>            Local cell IDs of the target local mesh object which
  !>            overlap with source local mesh cells. This array
  !>            should have dimensions of
  !>            [number of target cells for each source cell,
  !>             number of source cells].
  subroutine add_local_mesh_map( self,                  &
                                 target_local_mesh_id, &
                                 map )

    implicit none

    class(local_mesh_type), intent(inout) :: self
    integer(i_def), intent(in) :: target_local_mesh_id
    integer(i_def), intent(in) :: map(:,:,:)

    integer(i_def) :: source_local_mesh_id

    source_local_mesh_id = self%get_id()

    call self%local_mesh_maps% &
                          add_local_mesh_map( source_local_mesh_id, &
                                              target_local_mesh_id, &
                                              map )

  end subroutine add_local_mesh_map

  !==============================================================================
  !> @brief     Returns a pointer to a local_mesh_map object which maps local
  !>            cell IDs in the target local mesh to local cell IDs in the
  !>            source local mesh.
  !> @param[in] source_local_mesh_id  ID of source local mesh object of
  !>                                  requested local_mesh_map object.
  !> @param[in] target_local_mesh_id  ID of target local mesh object of
  !>                                  requested local_mesh_map object.
  !> @return    local_mesh_map_type   A pointer to a local_mesh_map object.
  function get_local_mesh_map( self,                  &
                               target_local_mesh_id ) &
                       result( local_mesh_map )

    implicit none

    class(local_mesh_type), intent(in) :: self
    integer(i_def),         intent(in) :: target_local_mesh_id
    type(local_mesh_map_type), pointer :: local_mesh_map

    integer(i_def) :: source_local_mesh_id

    source_local_mesh_id = self%get_id()

    local_mesh_map => self%local_mesh_maps% &
                          get_local_mesh_map( source_local_mesh_id, &
                                              target_local_mesh_id )

  end function get_local_mesh_map


  !==============================================================================
  !> @brief     Returns a pointer to the local_mesh_map collection object
  !>            contained in this local mesh object.
  !> @return    local_mesh_maps  Pointer to mesh_map_collection
  !>                             which contains intergrid maps where
  !>                             this mesh is the source mesh.
  !>
  function get_mesh_maps( self ) result( local_mesh_maps )

    implicit none

    class(local_mesh_type), intent(in),   target  :: self
    type(local_mesh_map_collection_type), pointer :: local_mesh_maps

    nullify(local_mesh_maps)

    local_mesh_maps => self%local_mesh_maps

  end function get_mesh_maps



  !==============================================================================
  !> @brief   Populates a <ugrid_2d_type> object with this local mesh object's
  !>          information.
  !> @details An <ugrid_2d_type> is passed in and populated using the
  !>          <ugrid_2d_type> `set_` methods.
  !> @param[in out] ugrid_2d  The <ugrid_2d_type> object to populate as a
  !>                          local mesh.
  !>
  !> @todo    This method is not preferred, though is used to break a circular
  !>          dependency which would result otherwise. This method could be
  !>          removed if the circular dependency can be resolved.
  !>
  !==============================================================================
  subroutine as_ugrid_2d( self, ugrid_2d )

    use ugrid_2d_mod, only: ugrid_2d_type

    implicit none

    class (local_mesh_type), intent(in) :: self

    type (ugrid_2d_type), intent(inout)  :: ugrid_2d

    character(str_def) :: geometry
    character(str_def) :: coord_sys
    character(str_def) :: topology
    character(str_def) :: units_xy(2)
    real(r_def)        :: factor


    if (self%is_geometry_spherical())    geometry  = 'spherical'
    if (self%is_geometry_planar())       geometry  = 'planar'
    if (self%is_coord_sys_ll())          coord_sys = 'll'
    if (self%is_coord_sys_xyz())         coord_sys = 'xyz'
    if (self%is_topology_periodic())     topology  = 'periodic'
    if (self%is_topology_channel())      topology  = 'channel'
    if (self%is_topology_non_periodic()) topology  = 'non_periodic'

    call ugrid_2d%set_dimensions                             &
            ( num_nodes          = self%n_unique_vertices,   &
              num_edges          = self%n_unique_edges,      &
              num_faces          = self%num_cells_in_layer + &
                                   self%num_ghost,           &
              num_nodes_per_face = self%nverts_per_cell,     &
              num_edges_per_face = self%nedges_per_cell,     &
              num_nodes_per_edge = self%nverts_per_edge )

    if (trim(self%coord_units_xy(1)) == 'radians' .and. &
        trim(self%coord_units_xy(2)) == 'radians') then
      factor = radians_to_degrees
      units_xy(1) = 'degrees_east'
      units_xy(2) = 'degrees_north'
    else
      factor = 1.0_r_def
      units_xy(:) = self%coord_units_xy(:)
    end if

    call ugrid_2d%set_coords( node_coords         = factor * self%vert_coords, &
                              north_pole          = factor * self%north_pole,  &
                              null_island         = factor * self%null_island, &
                              equatorial_latitude =                            &
                                            factor * self%equatorial_latitude, &
                              coord_sys           = coord_sys,                 &
                              units_xy            = units_xy )

    call ugrid_2d%set_connectivity( nodes_on_faces=self%vert_on_cell, &
                                    edges_on_faces=self%edge_on_cell, &
                                    faces_on_faces=self%cell_next,    &
                                    nodes_on_edges=self%vert_on_edge, &
                                    void_cell=self%void_cell )

    call ugrid_2d%set_partition_data( self%vert_cell_owner,  &
                                      self%edge_cell_owner,  &
                                      self%num_inner,        &
                                      self%num_halo,         &
                                      self%last_inner_cell,  &
                                      self%last_halo_cell,   &
                                      self%global_cell_id,   &
                                      self%vert_on_cell_gid, &
                                      self%edge_on_cell_gid )

    call ugrid_2d%set_metadata(                                &
              mesh_name          = self%mesh_name,             &
              geometry           = geometry,                   &
              topology           = topology,                   &
              npanels            = self%npanels,               &
              constructor_inputs = self%constructor_inputs,    &
              ncells_global_mesh = self%ncells_global_mesh,    &
              max_stencil_depth  = self%max_stencil_depth,     &
              domain_extents     = factor*self%domain_extents, &
              rim_depth          = self%rim_depth,             &
              inner_depth        = self%inner_depth,           &
              halo_depth         = self%halo_depth,            &
              num_edge           = self%num_edge,              &
              last_edge_cell     = self%last_edge_cell,        &
              num_ghost          = self%num_ghost,             &
              last_ghost_cell    = self%last_ghost_cell,       &
              nmaps              = self%ntarget_meshes,        &
              target_mesh_names  = self%target_mesh_names )

    call ugrid_2d%set_mesh_maps( self%local_mesh_maps )

    return
  end subroutine as_ugrid_2d


  !-------------------------------------------------------------------------------
  !> @brief      Queries the mesh names that this local mesh has
  !>             inter-grid maps to.
  !> @param[out] target_mesh_names  Names of target meshes that this
  !>                                mesh (source) has maps to.
  !>
  subroutine get_target_mesh_names(self, target_mesh_names)

    implicit none

    class(local_mesh_type), intent(in) :: self

    character(str_def), intent(out), allocatable :: target_mesh_names(:)

    if (allocated(self%target_mesh_names)) then
      if (allocated( target_mesh_names )) deallocate(target_mesh_names)
      allocate( target_mesh_names, source=self%target_mesh_names)
    end if

  end subroutine get_target_mesh_names


  !-------------------------------------------------------------------------------
  !> @brief  Queries the ID value used for void cells. These are invalid cells
  !>         in the cell-<element> connectivity.
  !> @return cell_id  The ID value used for null cell-connectivity.
  !>
  function get_void_cell (self) result(void_cell)
    implicit none

    class (local_mesh_type), intent(in) :: self

    integer(i_def) :: void_cell

    void_cell = self%void_cell

  end function get_void_cell


  !-------------------------------------------------------------------------------
  !> @brief  Returns the domain extents of the global model.
  !> @return global_domain_extents  Principal coordinates that describe
  !>                                the domain shape.
  !>
  function get_global_domain_extents(self) result(global_domain_extents)

    implicit none

    class (local_mesh_type), intent(in) :: self

    real(r_def) :: global_domain_extents(2,4)

    global_domain_extents(:,:) = self%domain_extents(:,:)

  end function get_global_domain_extents


  !-------------------------------------------------------------------------------
  ! Performs a binary search through the given integer array. PRIVATE function.
  !-------------------------------------------------------------------------------
  ! Details: Performs a binary search through the given array looking for a
  !          particular entry and returns the index of the entry found or -1 if no
  !          matching entry can't be found. The values held in
  !          "array_to_be_searched" must be in numerically increasing order.
  ! Input:   array_to_be_searched  The array that will be searched for the given entry
  !          value_to_find         The entry that is to be searched for
  !-------------------------------------------------------------------------------
  pure function binary_search( array_to_be_searched, value_to_find ) result ( id )

    implicit none

    integer(i_def), intent(in) :: array_to_be_searched( : )
    integer(i_def), intent(in) :: value_to_find
    integer(i_def)             :: bot, top  ! Lower and upper index limits between which to search for the value
    integer(i_def)             :: id        ! Next index for refining the search. If an entry is found this will
                                            ! contain the index of the match

    ! Set bot and top to be the whole array to begin with
    bot = 1
    top = size(array_to_be_searched)

    search: do
      ! If top is lower than bot then there is no more array to be searched
      if(top < bot) exit search
      ! Refine the search
      id = (bot+top)/2
      if(array_to_be_searched(id) == value_to_find)then  ! found matching entry
        return
      else if(array_to_be_searched(id) < value_to_find)then ! entry has to be between id and top
        bot = id + 1
      else ! entry has to be between bot and id
        top = id - 1
      endif
    end do search

    ! Didn't find a match - return failure code
    id = -1

  end function binary_search

end module local_mesh_mod
