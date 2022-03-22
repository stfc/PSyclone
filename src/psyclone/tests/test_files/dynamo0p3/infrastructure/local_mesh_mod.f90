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
                             i_native, emdi
  use global_mesh_mod, only: global_mesh_type

  use linked_list_data_mod,           only: linked_list_data_type
  use local_mesh_map_collection_mod,  only: local_mesh_map_collection_type
  use local_mesh_map_mod,             only: local_mesh_map_type
  use log_mod,                        only: log_event, log_scratch_space,     &
                                            LOG_LEVEL_ERROR, LOG_LEVEL_TRACE, &
                                            LOG_LEVEL_INFO
  use partition_mod,                  only: partition_type

  implicit none

  private

  integer(i_native), parameter :: spherical_domain = 601
  integer(i_native), parameter :: planar_domain    = 602

  integer(i_native), parameter :: non_periodic_domain = 701
  integer(i_native), parameter :: channel_domain      = 702
  integer(i_native), parameter :: periodic_domain     = 703

  integer(i_native), parameter :: lon_lat_coords = 801
  integer(i_native), parameter :: xyz_coords     = 802

  type, extends(linked_list_data_type), public :: local_mesh_type

    private

  ! Tag name of mesh
    character(str_def) :: mesh_name
  ! Domain surface geometry
    integer(i_native)  :: geometry = emdi
  ! Domain boundaries topology
    integer(i_native)  :: topology = emdi
  ! Co-ordinate system used to specify node locations
    integer(i_native)  :: coord_sys = emdi
  ! number of vertices on each cell
    integer(i_def)     :: nverts_per_cell
  ! number of vertices on each edge
    integer(i_def)     :: nverts_per_edge
  ! number of edges on each cell
    integer(i_def)     :: nedges_per_cell
  ! number of cells in a 2d slice of the local partition
  ! (not inc ghost cells).
    integer(i_def)     :: num_cells_in_layer
  ! A List of global cell ids known to this local mesh, ordered with inner
  ! cells first followed by the edge cells and finally the halo cells ordered
  ! by depth of halo
    integer(i_def), allocatable :: global_cell_id( : )
  ! Number of unique vertices in the local mesh
    integer(i_def) :: n_unique_vertices
  ! Number of unique edges in the local mesh
    integer(i_def) :: n_unique_edges
  ! Horizontal coords of vertices in local domain
    real(r_def), allocatable    :: vert_coords(:,:)
  ! Local ids of vertices connected to local 2d cell
    integer(i_def), allocatable :: vert_on_cell(:,:)
  ! Local ids of edges connected to local 2d cell.
    integer(i_def), allocatable :: edge_on_cell(:,:)
  ! Global ids of vertices connected to local 2d cell
    integer(i_def), allocatable :: vert_on_cell_gid(:,:)
  ! Global ids of edges connected to local 2d cell.
    integer(i_def), allocatable :: edge_on_cell_gid(:,:)
  ! Cell that "owns" each vertex
    integer(i_def), allocatable :: vert_cell_owner(:)
  ! Cell that "owns" each edge
    integer(i_def), allocatable :: edge_cell_owner(:)
  ! Local domain cell to cell lid connectivities
    integer(i_def), allocatable :: cell_next(:,:)
  ! A list of the ranks that own all the cells known to this partition
  ! held in the order of cells in the <code>global_cell_id</code> array
    integer(i_def), allocatable :: cell_owner( : )
  ! The number of "inner" cells in the <code>global_cell_id</code> list -
  ! one entry for each depth of inner halo
    integer(i_def), allocatable :: num_inner( : )
  ! The index of the last "inner" cell in the <code>global_cell_id</code> list -
  ! one entry for each depth of inner halo
    integer(i_def), allocatable :: last_inner_cell( : )
  ! The depth to which inner halos are generated
    integer(i_def)              :: inner_depth
  ! The number of "edge" cells in the <code>global_cell_id</code> list
    integer(i_def)              :: num_edge
  ! The index of the last "edge" cell in the <code>global_cell_id</code> list
    integer(i_def)              :: last_edge_cell
  ! The number of "halo" cells in the <code>global_cell_id</code> list -
  ! one entry for each depth of halo
    integer(i_def), allocatable :: num_halo( : )
  ! The index of the last "halo" cell in the <code>global_cell_id</code> list -
  ! one entry for each depth of halo
    integer(i_def), allocatable :: last_halo_cell( : )
  ! The depth to which halos are generated
    integer(i_def)              :: halo_depth
  ! The number of "ghost" cells in the <code>global_cell_id</code> list
    integer(i_def)              :: num_ghost
  ! The index of the last "ghost" cell in the <code>global_cell_id</code> lis
    integer(i_def)              :: last_ghost_cell
  ! Collection of local mesh maps associated with this mesh
    type(local_mesh_map_collection_type), allocatable :: &
                                   local_mesh_map_collection
  ! Number of panels in the global mesh
    integer(i_def)              :: npanels

  ! Number of cells in the global mesh
    integer(i_def)              :: ncells_global_mesh

  ! Max stencil depth supported by this mesh partition
    integer(i_def)              :: max_stencil_depth

  contains
    procedure, public  :: initialise_full
    procedure, public  :: initialise_unit_test
    generic            :: initialise => initialise_full, &
                                        initialise_unit_test
    procedure, public  :: init_cell_owner
    procedure, public  :: clear

    procedure, public  :: get_mesh_name
    procedure, public  :: get_nverts_per_cell
    procedure, public  :: get_nverts_per_edge
    procedure, public  :: get_nedges_per_cell
    procedure, public  :: get_num_cells_in_layer
    procedure, public  :: get_edge_gid_on_cell
    procedure, public  :: get_vert_gid_on_cell
    procedure, public  :: get_vert_coords
    procedure, public  :: get_n_unique_vertices
    procedure, public  :: get_n_unique_edges
    procedure, public  :: get_edge_on_cell
    procedure, public  :: get_vert_on_cell
    procedure, public  :: get_vert_cell_owner
    procedure, public  :: get_edge_cell_owner
    procedure, public  :: get_cell_next
    procedure, public  :: get_inner_depth
    procedure, public  :: get_num_cells_inner
    procedure, public  :: get_last_inner_cell
    procedure, public  :: get_num_cells_edge
    procedure, public  :: get_last_edge_cell
    procedure, public  :: get_halo_depth
    procedure, public  :: get_num_cells_halo
    procedure, public  :: get_last_halo_cell
    procedure, public  :: get_num_cells_ghost
    procedure, public  :: get_cell_owner
    procedure, public  :: get_num_panels_global_mesh
    procedure, public  :: get_ncells_global_mesh
    procedure, public  :: get_max_stencil_depth
    procedure, public  :: get_gid_from_lid
    procedure, public  :: get_lid_from_gid
    procedure, public  :: add_local_mesh_map
    procedure, public  :: get_local_mesh_map

    procedure, public :: is_geometry_spherical
    procedure, public :: is_geometry_planar
    procedure, public :: is_topology_non_periodic
    procedure, public :: is_topology_channel
    procedure, public :: is_topology_periodic
    procedure, public :: is_coord_sys_xyz
    procedure, public :: is_coord_sys_ll

    final :: local_mesh_destructor
  end type local_mesh_type

  !> Counter variable to keep track of the next local mesh id number to uniquely
  !! identify each different mesh
  integer(i_def), save :: local_mesh_id_counter = 0

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Initialises a local mesh object from global mesh and partition
  !>        objects
  !>
  !> This local mesh object holds the connectivities which fully describe
  !> the 2D topology of the local (partitionied) mesh.
  !>
  !> @param [in] global_mesh   Global mesh object on which the partition is
  !>                           applied
  !> @param [in] partition     Partition object to base the local Mesh on
  !> @param [in, optional]
  !>             mesh_name     Mesh tag name to use for this mesh. If omitted,
  !>                           the global mesh name it is based on will be used.
  !> @return New local_mesh_type object.
  !>
  subroutine initialise_full ( self,        &
                               global_mesh, &
                               partition,   &
                               mesh_name )
    implicit none
    class(local_mesh_type), intent(out)          :: self
    type(global_mesh_type), intent(in), pointer  :: global_mesh
    type(partition_type),   intent(in)           :: partition
    character(str_def),     intent(in), optional :: mesh_name

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

    logical (l_def)                              :: new_unique_vertex
    logical (l_def)                              :: new_unique_edge

    ! Set name from either the given name - or the name of the global mesh
    if (present(mesh_name)) then
      self%mesh_name = mesh_name
    else
      self%mesh_name = global_mesh%get_mesh_name()
    end if


    ! Inherit mesh properties from the parent global mesh
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


    ! Extract the info that makes up a local mesh from the
    ! global mesh and partition
    local_mesh_id_counter = local_mesh_id_counter + 1
    call self%set_id( local_mesh_id_counter )

    self%nverts_per_cell = global_mesh%get_nverts_per_cell()
    self%nverts_per_edge = global_mesh%get_nverts_per_edge()
    self%nedges_per_cell = global_mesh%get_nedges_per_cell()
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

    ! Allocate and init arrays to hold entity connectivities

    ! Cells next to a cell
    allocate( self%cell_next(self%nedges_per_cell, self%last_ghost_cell) )
    do cell_lid = 1, self%last_ghost_cell
      cell_gid = self%get_gid_from_lid(cell_lid)
      call global_mesh%get_cell_next( cell_gid, self%cell_next(:,cell_lid) )
      ! Convert the cell_next gids into lids
      do edge = 1, self%nedges_per_cell
        if (self%get_lid_from_gid(self%cell_next(edge,cell_lid)) > 0) then
          self%cell_next(edge,cell_lid) = &
                 self%get_lid_from_gid(self%cell_next(edge,cell_lid))
        else
          ! If lid is zero (or -ve) cell next is outside domain - so set to zero
          self%cell_next(edge,cell_lid) = 0
        end if
      end do
    end do

    ! Vertices on a cell
    allocate( self%vert_on_cell_gid(self%nverts_per_cell, self%last_ghost_cell) )
    allocate( self%vert_on_cell(self%nverts_per_cell, self%last_ghost_cell) )
    allocate( tmp_list(self%last_ghost_cell*self%nverts_per_cell) )
    self%n_unique_vertices = 0
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

    ! Edges on a cell
    allocate( self%edge_on_cell_gid(self%nedges_per_cell, self%last_ghost_cell) )
    allocate( self%edge_on_cell(self%nedges_per_cell, self%last_ghost_cell) )
    allocate( tmp_list(self%last_ghost_cell*self%nedges_per_cell) )
    self%n_unique_edges = 0
    do cell_lid = 1, self%last_ghost_cell
      cell_gid = self%get_gid_from_lid(cell_lid)
      call global_mesh%get_edge_on_cell( cell_gid, &
                                         self%edge_on_cell_gid(:,cell_lid) )
      do edge = 1, self%nedges_per_cell
        new_unique_edge = .true.
        do counter = 1, self%n_unique_edges
          if (tmp_list(counter) == self%edge_on_cell_gid(edge,cell_lid)) then
            new_unique_edge = .false.
            self%edge_on_cell(edge,cell_lid) = counter
            exit
          end if
        end do
        if ( new_unique_edge ) then
          self%n_unique_edges = self%n_unique_edges + 1
          tmp_list(self%n_unique_edges) = &
                             self%edge_on_cell_gid(edge, cell_lid)
          self%edge_on_cell(edge,cell_lid) = self%n_unique_edges
        end if
      end do
    end do
    allocate(edge_lid_gid_map(self%n_unique_edges))
    edge_lid_gid_map(:) = tmp_list(1:self%n_unique_edges)
    deallocate(tmp_list)

    ! Set cell ownership for each vertex
    allocate( self%vert_cell_owner(self%n_unique_vertices) )
    do vertex=1,self%n_unique_vertices
      self%vert_cell_owner(vertex) = &
          self%get_lid_from_gid( &
            global_mesh%get_vert_cell_owner(vert_lid_gid_map(vertex)) )
    end do

    ! Set cell ownership for each edge
    allocate( self%edge_cell_owner(self%n_unique_edges) )
    do edge=1,self%n_unique_edges
      self%edge_cell_owner(edge) = &
          self%get_lid_from_gid( &
            global_mesh%get_edge_cell_owner(edge_lid_gid_map(edge)) )
    end do

    allocate( self%vert_coords(3,self%n_unique_vertices) )
    do vertex = 1, self%n_unique_vertices
      ! Get coords of vertices
      vert_gid = vert_lid_gid_map(vertex)
      call global_mesh%get_vert_coords( vert_gid, self%vert_coords(:,vertex) )
    end do

    if (.not. allocated(self%local_mesh_map_collection) ) &
        allocate ( self%local_mesh_map_collection,        &
              source = local_mesh_map_collection_type() )

    self%npanels = partition%get_num_panels_global_mesh()

    self%ncells_global_mesh = global_mesh%get_ncells()

    deallocate( vert_lid_gid_map )
    deallocate( edge_lid_gid_map )

  end subroutine initialise_full

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Initialises a local mesh object for use in unit testing
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

    local_mesh_id_counter = local_mesh_id_counter + 1
    call self%set_id( local_mesh_id_counter )

    self%nverts_per_cell = 4
    self%nverts_per_edge = 2
    self%nedges_per_cell = 4
    self%num_cells_in_layer = 9
    self%n_unique_vertices = 16
    self%n_unique_edges = 24

    allocate( self%global_cell_id(9) )
    self%global_cell_id = [1,2,3,4,5,6,7,8,9]

    self%inner_depth = 1
    allocate( self%num_inner(self%inner_depth) )
    allocate( self%last_inner_cell(self%inner_depth) )
    self%num_inner(1) = 9
    self%last_inner_cell(1) = 9

    self%num_edge = 0
    self%last_edge_cell = 9

    self%halo_depth  = 3
    allocate( self%num_halo(self%halo_depth) )
    self%num_halo(1) = 0
    self%num_halo(2) = 0
    self%num_halo(3) = 0
    allocate( self%last_halo_cell(self%halo_depth) )
    self%last_halo_cell(1) = 9
    self%last_halo_cell(2) = 9
    self%last_halo_cell(3) = 9

    self%num_ghost = 0

    allocate( self%vert_cell_owner(self%n_unique_vertices) )
    self%vert_cell_owner(:) = [ 8, 9, 6, 1, &
                                5, 3, 2, 4, &
                                6, 4, 7, 9, &
                                7, 8, 9, 9 ]
    allocate( self%edge_cell_owner(self%n_unique_edges) )
    self%edge_cell_owner(:) = [ 1, 8, 6, 1, 2, 5, &
                                3, 2, 4, 3, 6, 4, &
                                4, 5, 7, 9, 6, 7, &
                                7, 8, 8, 9, 9, 9 ]

    allocate( self%cell_next(self%nedges_per_cell, self%num_cells_in_layer) )
    self%cell_next(:,1) = [3, 7, 2, 4]
    self%cell_next(:,2) = [1, 8, 3, 5]
    self%cell_next(:,3) = [2, 9, 1, 6]
    self%cell_next(:,4) = [6, 1, 5, 7]
    self%cell_next(:,5) = [4, 2, 6, 8]
    self%cell_next(:,6) = [5, 3, 4, 9]
    self%cell_next(:,7) = [9, 4, 8, 1]
    self%cell_next(:,8) = [7, 5, 9, 2]
    self%cell_next(:,9) = [8, 6, 7, 3]

    allocate( self%vert_on_cell(self%nverts_per_cell, self%num_cells_in_layer) )
    self%vert_on_cell(:, 1) = [ 1,  2,  3,  4]
    self%vert_on_cell(:, 2) = [ 2,  5,  6,  3]
    self%vert_on_cell(:, 3) = [ 5,  1,  4,  6]
    self%vert_on_cell(:, 4) = [ 4,  3,  7,  8]
    self%vert_on_cell(:, 5) = [ 3,  6,  9,  7]
    self%vert_on_cell(:, 6) = [ 6,  4,  8,  9]
    self%vert_on_cell(:, 7) = [ 8,  7,  2,  1]
    self%vert_on_cell(:, 8) = [ 7,  9,  5,  2]
    self%vert_on_cell(:, 9) = [ 9,  8,  1,  5]

    allocate( self%edge_on_cell(self%nedges_per_cell, self%num_cells_in_layer) )
    self%edge_on_cell(:, 1) = [ 1,  2,  3,  4]
    self%edge_on_cell(:, 2) = [ 3,  5,  6,  7]
    self%edge_on_cell(:, 3) = [ 6,  8,  1,  9]
    self%edge_on_cell(:, 4) = [10,  4, 11, 12]
    self%edge_on_cell(:, 5) = [11,  7, 13, 14]
    self%edge_on_cell(:, 6) = [13,  9, 10, 15]
    self%edge_on_cell(:, 7) = [16, 12, 17,  2]
    self%edge_on_cell(:, 8) = [17, 14, 18,  5]
    self%edge_on_cell(:, 9) = [18, 15, 16,  8]

    allocate( self%vert_on_cell_gid(self%nverts_per_cell, self%num_cells_in_layer) )
    self%vert_on_cell_gid(:, 1) = [ 1,  2,  3,  4]
    self%vert_on_cell_gid(:, 2) = [ 2,  5,  6,  3]
    self%vert_on_cell_gid(:, 3) = [ 5,  1,  4,  6]
    self%vert_on_cell_gid(:, 4) = [ 4,  3,  7,  8]
    self%vert_on_cell_gid(:, 5) = [ 3,  6,  9,  7]
    self%vert_on_cell_gid(:, 6) = [ 6,  4,  8,  9]
    self%vert_on_cell_gid(:, 7) = [ 8,  7,  2,  1]
    self%vert_on_cell_gid(:, 8) = [ 7,  9,  5,  2]
    self%vert_on_cell_gid(:, 9) = [ 9,  8,  1,  5]

    allocate( self%edge_on_cell_gid(self%nedges_per_cell, self%num_cells_in_layer) )
    self%edge_on_cell_gid(:, 1) = [ 1,  2,  3,  4]
    self%edge_on_cell_gid(:, 2) = [ 3,  5,  6,  7]
    self%edge_on_cell_gid(:, 3) = [ 6,  8,  1,  9]
    self%edge_on_cell_gid(:, 4) = [10,  4, 11, 12]
    self%edge_on_cell_gid(:, 5) = [11,  7, 13, 14]
    self%edge_on_cell_gid(:, 6) = [13,  9, 10, 15]
    self%edge_on_cell_gid(:, 7) = [16, 12, 17,  2]
    self%edge_on_cell_gid(:, 8) = [17, 14, 18,  5]
    self%edge_on_cell_gid(:, 9) = [18, 15, 16,  8]

    allocate( self%cell_owner(self%num_cells_in_layer+self%num_ghost) )
    self%cell_owner = 0

    self%npanels = 1

  end subroutine initialise_unit_test

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Calculate ownership of all cells known to this local mesh
  !>
  !> An array representing all cells known to the local mesh has all the
  !> owned cells filled with the local rank ID (on every processor in
  !> parallel). When a halo swap is performed on this array, the array on
  !> every processor has its halo cells filled with their owner.
  !>
  subroutine init_cell_owner(self)
    use mpi_mod, only: generate_redistribution_map, get_mpi_datatype, &
                       get_comm_rank
    !use yaxt,    only: xt_redist, xt_redist_s_exchange

    implicit none
    class (local_mesh_type), intent(inout) :: self
    !type(xt_redist) :: redist
    integer(i_def) :: cell
    integer(i_def) :: i
    integer(i_def) :: total_inners
    integer(i_def) :: halo_start, halo_finish
    integer(i_def) :: local_rank

    allocate( self%cell_owner(self%num_cells_in_layer+self%num_ghost) )

    ! Work out the boundary between owned and halo cells
    total_inners=0
    do i=1,self%inner_depth
      total_inners=total_inners+self%num_inner(i)
    end do
    halo_start  = total_inners+self%num_edge+1
    halo_finish = self%get_num_cells_in_layer()+self%get_num_cells_ghost()
    !If this is a serial run (no halos), halo_start is out of bounds - so fix it
    if(halo_start > self%get_num_cells_in_layer())then
      halo_start  = self%get_num_cells_in_layer()
      halo_finish = self%get_num_cells_in_layer() - 1
    end if

    !Get the redistribution map object for halo exchanging the cell owners array
    !redist = generate_redistribution_map( &
    !  int(self%global_cell_id(1:total_inners+self%num_edge),kind=i_halo_index),&
    !  int(self%global_cell_id( halo_start:halo_finish ),kind=i_halo_index), &
    !  get_mpi_datatype( integer_type, i_def ) )

    ! Set ownership of all inner and edge cells to the local rank id
    ! - halo cells are unset
    local_rank = get_comm_rank()
    do cell = 1,total_inners+self%num_edge
      self%cell_owner(cell)=local_rank
    end do

    ! Perform the halo swap
    !call xt_redist_s_exchange(redist, self%cell_owner, self%cell_owner)

  end subroutine init_cell_owner

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Fortran destructor, called when object goes out of scope
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

    if( allocated( self%global_cell_id ) )  deallocate( self%global_cell_id )
    if( allocated( self%cell_next ) )       deallocate( self%cell_next )
    if( allocated( self%cell_owner ) )      deallocate( self%cell_owner )
    if( allocated( self%num_inner ) )       deallocate( self%num_inner )
    if( allocated( self%last_inner_cell ) ) deallocate( self%last_inner_cell )
    if( allocated( self%num_halo ) )        deallocate( self%num_halo )
    if( allocated( self%last_halo_cell ) )  deallocate( self%last_halo_cell )
    if( allocated( self%vert_cell_owner) )  deallocate( self%vert_cell_owner)
    if( allocated( self%edge_cell_owner) )  deallocate( self%edge_cell_owner)
    return
  end subroutine clear

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Returns mesh tag name.
  !> @return mesh_name  Tag name of mesh
  !>
  function get_mesh_name( self ) result ( mesh_name )
    implicit none
    class(local_mesh_type), intent(in) :: self
    character(str_def) :: mesh_name

    mesh_name = self%mesh_name

  end function get_mesh_name


  !---------------------------------------------------------------------------
  !> @brief  Queries if the local mesh domain geometry is spherical
  !>
  !> @return answer .true. for a spherical domain surface
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
  !> @return answer .true. for a flat domain surface
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
  !> @return answer .true. for a domain which is non-periodic
  !>
  function is_topology_non_periodic( self ) result ( answer )

    implicit none

    class(local_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%topology == non_periodic_domain )

  end function is_topology_non_periodic


  !---------------------------------------------------------------------------
  !> @brief  Queries if the local mesh topology specifies a domain where there
  !>         is only one pair of entry/exit boundaries
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
  !> @return answer .true. for a domain which is periodic
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
  !> @return answer .true. if nodes are specified in Cartesian co-ordinates
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
  !> @return answer .true. if nodes are specified in spherical co-ordinates
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

  !> @details Returns the global edge id on the local cell
  !> @param [in] iedge    The index of the edge whose global id is requested
  !> @param [in] icell    The local id of the cell on which the edge is located
  !> @return              Global id of the selected edge
  !>
  function get_edge_gid_on_cell(self, iedge, icell) result (edge_gid)

    ! Return global edge id on local cell

    implicit none
    class(local_mesh_type), intent(in) :: self
    integer(i_def),         intent(in) :: iedge   ! Index of edge required
    integer(i_def),         intent(in) :: icell   ! Local cell id
    integer(i_def)                     :: edge_gid

    edge_gid = self%edge_on_cell_gid(iedge, icell)

  end function get_edge_gid_on_cell

  !> @details Returns the global vertex id on the local cell
  !> @param [in] ivert    The index of the vertex whose global id is requested
  !> @param [in] icell    The local id of the cell on which the vertex
  !>                      is located
  !> @return              Global id of the selected vertex
  !>
  function get_vert_gid_on_cell(self, ivert, icell) result (vert_gid)

    ! Returns global vertex id on local cell

    implicit none
    class(local_mesh_type), intent(in) :: self
    integer(i_def),         intent(in) :: ivert   ! Index of vertex required
    integer(i_def),         intent(in) :: icell   ! Local cell id
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
  !> @brief Gets the number of unique vertices in the local domain
  !>
  !> @return The number of unique vertices in the local domain
  function get_n_unique_vertices( self ) result ( n_unique_vertices )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def) :: n_unique_vertices

    n_unique_vertices = self%n_unique_vertices

  end function get_n_unique_vertices

  !---------------------------------------------------------------------------
  !> @brief Gets the number of unique edges in the local domain
  !>
  !> @return The number of unique edges in the local domain
  function get_n_unique_edges( self ) result ( n_unique_edges )

    implicit none

    class(local_mesh_type), intent(in) :: self

    integer(i_def) :: n_unique_edges

    n_unique_edges = self%n_unique_edges

  end function get_n_unique_edges

  !> @details Returns the local edge id on the local cell
  !> @param [in] iedge    The index of the edge whose local id is requested
  !> @param [in] icell    The local id of the cell on which the edge is located
  !> @return              Local id of the select edge
  !>
  function get_edge_on_cell(self, iedge, icell) result (edge_lid)

    ! Return local edge id on local cell

    implicit none
    class(local_mesh_type), intent(in) :: self
    integer(i_def),         intent(in) :: iedge   ! Index of edge required
    integer(i_def),         intent(in) :: icell   ! Local cell id
    integer(i_def)                     :: edge_lid

    edge_lid = self%edge_on_cell(iedge, icell)

  end function get_edge_on_cell

  !> @details Returns the local vertex id on the local cell
  !> @param [in] ivert    The index of the vertex whose local id is requested
  !> @param [in] icell    The local id of the cell on which the vertex
  !>                      is located
  !> @return              Local id of the selected vertex
  !>
  function get_vert_on_cell(self, ivert, icell) result (vert_lid)

    ! Returns local vertex id on local cell

    implicit none
    class(local_mesh_type), intent(in) :: self
    integer(i_def),         intent(in) :: ivert   ! Index of vertex required
    integer(i_def),         intent(in) :: icell   ! Local cell id
    integer(i_def)                     :: vert_lid

    vert_lid = self%vert_on_cell(ivert, icell)

  end function get_vert_on_cell

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the cell that a vertex has been allocated to
  !>
  !> @param[in] vert Local ID of the vertex
  !>
  !> @return Local cell ID that the vertex has been allocated to
  !>
  function get_vert_cell_owner ( self, vert ) result ( cell )

    implicit none
    class (local_mesh_type), intent(in)  :: self
    integer (i_def),         intent(in)  :: vert
    integer (i_def)                      :: cell

    cell = self%vert_cell_owner( vert )

  end function get_vert_cell_owner

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the cell that an edge has been allocated to
  !>
  !> @param[in] edge Local ID of the edge
  !>
  !> @return Local cell ID that the edge has been allocated to
  !>
  function get_edge_cell_owner ( self, edge ) result ( cell )

    implicit none
    class (local_mesh_type), intent(in)  :: self
    integer (i_def),         intent(in)  :: edge
    integer (i_def)                      :: cell

    cell = self%edge_cell_owner( edge )

  end function get_edge_cell_owner

  !---------------------------------------------------------------------------
  !> @brief Gets the local ids of the cells adjacent to a cell.
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
  !>          depth of inner halo in a 2d slice on the local partition.
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

    if( depth > self%inner_depth )then
      inner_cells = 0
    else
      inner_cells = self%num_inner(depth)
    end if

  end function get_num_cells_inner

  !---------------------------------------------------------------------------
  !> @brief Gets the index of the last cell in an inner halo.
  !>
  !> @details Returns the index of the last cell in a particular depth of
  !>          inner halo in a 2d slice on the local partition.
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

    if( depth > self%inner_depth )then
      last_inner_cell = 0
    else
      last_inner_cell = self%last_inner_cell(depth)
    end if

  end function get_last_inner_cell

  !---------------------------------------------------------------------------
  !> @brief Gets the total number of edge cells in a 2d slice on the local
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
  !> @brief Gets the index of the last edge cell in a 2d slice on the local
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
  !>          of halo in a 2d slice on the local partition.
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

    if( depth > self%halo_depth )then
      halo_cells = 0
    else
      halo_cells = self%num_halo(depth)
    end if

  end function get_num_cells_halo

  !---------------------------------------------------------------------------
  !> @brief Gets the index of the last cell in a halo.
  !>
  !> @details Returns the index of the last cell in a particular depth of halo
  !>          in a 2d slice on the local partition.
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

    if( depth > self%halo_depth )then
      last_halo_cell = 0
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
  !> @return Number of cells in the global mesh
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
  !> @brief Gets the local index of the cell on the local partition that
  !>        corresponds to the given global index.
  !>
  !> @param[in] gid Global index to search for on the local partition.
  !>
  !> @return Local index that corresponds to the given global index or -1 if
  !>         the cell with the given global index is not present of the local
  !>         partition
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
  ! exit if a match is found
  !
    implicit none

    class(local_mesh_type), intent(in) :: self
    integer(i_def),         intent(in) :: gid  ! global index

    integer(i_def) :: lid                     ! local index (returned)

    integer(i_def) :: num_searched            ! Num cells alreadt searched
    integer(i_def) :: depth                   ! loop counter over halo depths
    integer(i_def) :: start_search            ! start point for a search
    integer(i_def) :: end_search              ! end point for a search

    ! Set the default return code
    lid = -1
    ! If the supplied gid is not valid just return
    if(gid < 1) return

    num_searched = 0
    ! Search though the inner halo cells - looking for the gid
    end_search = 0
    do depth = self%inner_depth, 1, -1
      start_search = end_search + 1
      end_search = start_search + self%num_inner(depth) - 1

      lid = binary_search( self%global_cell_id( start_search:end_search ), gid )
      if(lid /= -1)then
        lid = lid + num_searched
        return
      end if
      num_searched = num_searched + self%num_inner(depth)
    end do

    ! Search though edge cells - looking for the gid
    start_search = end_search + 1
    end_search = start_search + self%num_edge - 1

    lid = binary_search( self%global_cell_id( start_search:end_search ), gid )
    if(lid /= -1)then
      lid = lid + num_searched
      return
    end if
    num_searched = num_searched + self%num_edge

    ! Search though halo cells - looking for the gid
    do depth = 1,self%halo_depth + 1
      start_search = end_search + 1
      if(depth <= self%halo_depth) then
        end_search = start_search + self%num_halo(depth) - 1
      else
        end_search = start_search + self%num_ghost - 1
      end if

      lid = binary_search( self%global_cell_id( start_search:end_search ), gid )
      if(lid /= -1)then
        lid = lid + num_searched
        return
      end if
      if(depth <= self%halo_depth) then
        num_searched = num_searched + self%num_halo(depth)
      end if
    end do

    ! No lid has been found in either the inner, edge, halo or ghost cells
    ! on this partition, so return with lid=-1
    return

  end function get_lid_from_gid

  !==============================================================================
  !> @brief     Adds a local_mesh_map object to the mesh map collection
  !.            held in this local mesh
  !> @param[in] target_local_mesh_id ID of the target local mesh
  !>                                  object for this local mesh map object.
  !> @param[in] map
  !>            Local cell ids of the target local mesh object which
  !>            overlap with source local mesh cells. This array
  !>            should have dimensions of
  !>            [number of target cells for each source cell,
  !>             number of source cells]
  subroutine add_local_mesh_map( self,                  &
                                 target_local_mesh_id, &
                                 map )

    implicit none

    class(local_mesh_type), intent(inout) :: self
    integer(i_def), intent(in) :: target_local_mesh_id
    integer(i_def), intent(in) :: map(:,:,:)

    integer(i_def) :: source_local_mesh_id

    source_local_mesh_id = self%get_id()

    call self%local_mesh_map_collection% &
                          add_local_mesh_map( source_local_mesh_id, &
                                              target_local_mesh_id, &
                                              map )

  end subroutine add_local_mesh_map

  !==============================================================================
  !> @brief     Returns a pointer to a local_mesh_map object which maps local
  !>            cell ids in the target local mesh to local cell ids in the
  !>            source local mesh.
  !> @param[in] source_local_mesh_id ID of source local mesh
  !>                                  object of requested local_mesh_map
  !>                                  object.
  !> @param[in] target_local_mesh_id ID of target local mesh
  !>                                  object of requested local_mesh_map
  !>                                  object.
  !> @return    local_mesh_map_type A pointer to a local_mesh_map object
  function get_local_mesh_map( self,                  &
                               target_local_mesh_id ) &
                       result( local_mesh_map )

    implicit none

    class(local_mesh_type), intent(in) :: self
    integer(i_def),         intent(in) :: target_local_mesh_id
    type(local_mesh_map_type), pointer :: local_mesh_map

    integer(i_def) :: source_local_mesh_id

    source_local_mesh_id = self%get_id()

    local_mesh_map => self%local_mesh_map_collection% &
                          get_local_mesh_map( source_local_mesh_id, &
                                              target_local_mesh_id )

  end function get_local_mesh_map

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
