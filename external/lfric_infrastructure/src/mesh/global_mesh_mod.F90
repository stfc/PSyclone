!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

!> @brief  Describes the cell ordering within the global mesh.
!>
!> @details This object holds the connectivities that fully
!>          describe the 2D topology of the global mesh.
!>
module global_mesh_mod

  use constants_mod,                  only: r_def, i_def, str_max_filename, &
                                            str_def, degrees_to_radians,    &
                                            rmdi, imdi,                     &
                                            cmdi, l_def, emdi,              &
                                            str_longlong

  use global_mesh_map_mod,            only: global_mesh_map_type
  use global_mesh_map_collection_mod, only: global_mesh_map_collection_type
  use linked_list_data_mod,           only: linked_list_data_type
  use log_mod,                        only: log_event, log_scratch_space, &
                                            LOG_LEVEL_ERROR,              &
                                            LOG_LEVEL_TRACE

  implicit none

  private

  integer(i_def), parameter :: spherical_domain = 301
  integer(i_def), parameter :: planar_domain    = 302

  integer(i_def), parameter :: non_periodic_domain = 401
  integer(i_def), parameter :: channel_domain      = 402
  integer(i_def), parameter :: periodic_domain     = 403

  integer(i_def), parameter :: lon_lat_coords = 501
  integer(i_def), parameter :: xyz_coords     = 502


  type, extends(linked_list_data_type), public :: global_mesh_type

    private

  !======================================================
  ! Mesh metadata information.
  !======================================================
  ! Tag name of mesh.
    character(str_def) :: mesh_name

  ! Flag to indicate if this a mesh represents coverage
  ! of a global model.
    logical(l_def) :: global_model = .false.

  ! Domain surface geometry.
    integer(i_def) :: geometry = emdi

  ! Domain boundaries topology.
    integer(i_def) :: topology = emdi

  ! Co-ordinate system used to specify node locations.
    integer(i_def) :: coord_sys = emdi

  ! Units used internal to this object.
    character(str_def) :: coord_units_xy(2) = cmdi

  ! Configuration inputs used to generate this global mesh object
    character(str_longlong) :: constructor_inputs = cmdi

  ! Principal coordinates that describe the domain shape.
  ! Units dependant on the geometry/coordinate system.
    real(r_def) :: domain_extents(2,4) = rmdi

  ! Real-world location of mesh North Pole,
  ! only valid for spherical geometry on lon-lat cordinate-system.
    real(r_def) :: north_pole(2) = rmdi

  ! Real-world location of mesh null island,
  ! only valid for spherical geometry on lon-lat cordinate-system.
    real(r_def) :: null_island(2) = rmdi

  ! Latitude of equator of mesh following stretching
  ! only valid for spherical geometry and periodic topology
    real(r_def) :: equatorial_latitude = rmdi

  ! Periodic in x/y-axes
    logical(l_def) :: periodic_xy(2) = [.false.,.false.]

  ! Rim depth is only relevant for LBC meshes which will have a LAM
  ! mesh as the parent.
    integer(i_def) :: rim_depth = imdi

  !======================================================
  ! Mesh topology information.
  !======================================================
  ! Horizontal coords of vertices in full domain.
    real(r_def), allocatable :: vert_coords(:,:)

  ! Full domain cell to cell connectivities.
    integer(i_def), allocatable :: cell_next_2d(:,:)

  ! Full domain vertices on a cell.
    integer(i_def), allocatable :: vert_on_cell_2d(:,:)

  ! Full domain cells that surround a vertex.
    integer(i_def), allocatable :: cell_on_vert_2d(:,:)

  ! Full domain edges on a cell.
    integer(i_def), allocatable :: edge_on_cell_2d(:,:)

  ! Full domain cells either side of an edge.
    integer(i_def), allocatable :: cell_on_edge_2d(:,:)

  ! Full domain list the cells that vertices are allocated to.
    integer(i_def), allocatable :: vert_cell_owner(:)

  ! Full domain list the cells that edges are allocated to.
    integer(i_def), allocatable :: edge_cell_owner(:)

  ! Total number of vertices in the full domain.
    integer(i_def) :: nverts = imdi

  ! Total number of edges in the full domain.
    integer(i_def) :: nedges = imdi

  ! total number of cells in full domain.
    integer(i_def) :: ncells = imdi

  ! number of vertices on each cell.
    integer(i_def) :: nverts_per_cell

  ! number of vertices on each edge.
    integer(i_def) :: nverts_per_edge

  ! number of edges on each cell.
    integer(i_def) :: nedges_per_cell

  ! maximum number of cells around a vertex.
    integer(i_def) :: max_cells_per_vertex

  ! Number of panels on the mesh to be constructed.
    integer(i_def) :: npanels

  ! ID used for invalid cells in connectivity.
    integer(i_def) :: void_cell

  !======================================================
  ! Intergrid map(s) information.
  !======================================================
  ! Number of intergrid mesh maps with this mesh as source.
    integer(i_def) :: ntarget_meshes

  ! Target of global mesh name.
    character(str_def), allocatable :: target_global_mesh_names(:)

  ! Collection of global mesh maps in global cell IDs
    type(global_mesh_map_collection_type), allocatable :: global_mesh_maps

  !-------------------------------------------------------------
  ! Supplementary data: Only held for outputting to file.
  ! These may not actually be used in the main model though may
  ! be required for output to the mesh file for supporting
  ! applications, e.g. LFRic Inputs, XIOS. May be relocated
  ! in future tickets.

  ! Horizontal coords of cells in full domain.
    real(r_def), allocatable :: cell_coords(:,:)

  ! Node IDs attached to edges.
    integer(i_def), allocatable :: vert_on_edge_2d(:,:)

  contains
    procedure, public :: get_mesh_name
    procedure, public :: get_npanels
    procedure, public :: get_mesh_periodicity
    procedure, public :: get_cell_id
    procedure, public :: get_cell_on_vert
    procedure, public :: get_cell_on_edge
    procedure, public :: get_void_cell
    procedure, public :: get_nverts
    procedure, public :: get_nedges
    procedure, public :: get_ncells
    procedure, public :: get_max_cells_per_vertex
    procedure, public :: get_edge_on_cell
    procedure, public :: get_vert_on_cell
    procedure, public :: get_edge_on_all_cells
    procedure, public :: get_vert_on_all_cells
    procedure, public :: get_nverts_per_cell
    procedure, public :: get_nverts_per_edge
    procedure, public :: get_nedges_per_cell
    procedure, public :: get_cell_next
    procedure, public :: get_all_cells_next
    procedure, public :: get_a_vert_coords
    procedure, public :: get_all_vert_coords
    generic,   public :: get_vert_coords => get_a_vert_coords, &
                                            get_all_vert_coords
    procedure, public :: get_a_cell_coords
    procedure, public :: get_all_cell_coords
    generic,   public :: get_cell_coords => get_a_cell_coords, &
                                            get_all_cell_coords
    procedure, public :: get_vert_cell_owner
    procedure, public :: get_edge_cell_owner
    procedure, public :: add_global_mesh_map
    procedure, public :: get_global_mesh_map
    procedure, public :: get_target_mesh_names
    procedure, public :: get_nmaps
    procedure, public :: get_mesh_maps

    procedure, public :: is_geometry_spherical
    procedure, public :: is_geometry_planar
    procedure, public :: is_topology_non_periodic
    procedure, public :: is_topology_channel
    procedure, public :: is_topology_periodic
    procedure, public :: is_coord_sys_xyz
    procedure, public :: is_coord_sys_ll

    procedure, public :: get_rim_depth
    procedure, public :: get_domain_extents
    procedure, public :: get_north_pole
    procedure, public :: get_null_island
    procedure, public :: get_equatorial_latitude
    procedure, public :: get_vert_on_edge
    procedure, public :: get_coord_units
    procedure, public :: get_constructor_inputs

    procedure, public :: clear

    final :: global_mesh_destructor

  end type global_mesh_type

  interface global_mesh_type
    module procedure global_mesh_constructor
    module procedure global_mesh_constructor_unit_test_data
  end interface

  ! -------------------------------------------------------------------------
  ! Module parameters.
  ! -------------------------------------------------------------------------

  ! Counter variable to keep track of the next mesh id number to uniquely
  ! identify each different mesh.
  integer(i_def), save :: global_mesh_id_counter = 0

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Constructs a global mesh object from mesh file and reference
  !>        element.
  !>
  !> This global mesh object holds the connectivities which fully describe
  !> the 2D topology of the mesh.
  !>
  !> @param[in] filename         Filename for global 2D mesh(es) ugrid file.
  !> @param[in] global_mesh_name Name of ugrid mesh topology to create
  !>                             global mesh object from.
  !>
  !> @return Freshly minted global_mesh_type object.
  !>
  function global_mesh_constructor( ugrid_mesh_data ) result(self)

    use ugrid_mesh_data_mod, only: ugrid_mesh_data_type

    implicit none

    type(ugrid_mesh_data_type), intent(in) :: ugrid_mesh_data

    type(global_mesh_type) :: self

    character(str_def) :: geometry
    character(str_def) :: topology
    character(str_def) :: coord_sys

    ! loop counter over entities (vertices or edges).
    integer(i_def) :: ientity

    call ugrid_mesh_data%get_data( self%mesh_name, &
                                   geometry, &
                                   topology, &
                                   coord_sys, &
                                   self%npanels, &
                                   self%nverts, &
                                   self%nedges, &
                                   self%ncells, &
                                   self%nverts_per_cell, &
                                   self%nverts_per_edge, &
                                   self%nedges_per_cell, &
                                   self%max_cells_per_vertex, &
                                   self%periodic_xy, &
                                   self%ntarget_meshes, &
                                   self%target_global_mesh_names, &
                                   self%vert_coords, &
                                   self%cell_coords, &
                                   self%coord_units_xy, &
                                   self%north_pole, &
                                   self%null_island, &
                                   self%equatorial_latitude, &
                                   self%constructor_inputs, &
                                   self%rim_depth, &
                                   self%domain_extents, &
                                   self%void_cell, &
                                   self%cell_next_2d, &
                                   self%vert_on_cell_2d, &
                                   self%edge_on_cell_2d, &
                                   self%vert_on_edge_2d )

    global_mesh_id_counter = global_mesh_id_counter + 1

    call self%set_id(global_mesh_id_counter)

    select case (trim(geometry))

    case ('spherical')
      self%geometry = spherical_domain

    case ('planar')
      self%geometry = planar_domain

    end select


    select case (trim(topology))

    case ('non_periodic')
      self%topology = non_periodic_domain

    case ('channel')
      self%topology = channel_domain

    case ('periodic')
      self%topology = periodic_domain

    end select


    select case (trim(coord_sys))

    case ('ll')
      self%coord_sys = lon_lat_coords

    case ('xyz')
      self%coord_sys = xyz_coords

    end select


    ! CF Standard for longitude/latitude is in degrees
    ! though many functions assume radians. Convert
    ! coords to radians before going any further into
    ! code.
    if (self%coord_sys == lon_lat_coords) then
      if (trim(self%coord_units_xy(1)) == 'degrees_east' .and. &
          trim(self%coord_units_xy(2)) == 'degrees_north') then

        self%vert_coords(:,:)    = degrees_to_radians * self%vert_coords(:,:)
        self%cell_coords(:,:)    = degrees_to_radians * self%cell_coords(:,:)
        self%north_pole(:)       = degrees_to_radians * self%north_pole(:)
        self%null_island(:)      = degrees_to_radians * self%null_island(:)
        self%equatorial_latitude = degrees_to_radians * self%equatorial_latitude
        self%domain_extents(:,:) = degrees_to_radians * self%domain_extents(:,:)

        self%coord_units_xy(:) = 'radians'

      end if
    end if

    allocate( self%cell_on_vert_2d( self%max_cells_per_vertex, self%nverts ) )
    call calc_cell_on_vertex( self%vert_on_cell_2d,      &
                              self%nverts_per_cell,      &
                              self%ncells,               &
                              self%cell_on_vert_2d,      &
                              self%max_cells_per_vertex, &
                              self%nverts,               &
                              self%void_cell )

    ! Populate cells either side of each edge.
    ! There can only ever be 2 cells incident on an edge (whatever the
    ! topography!)
    allocate( self%cell_on_edge_2d(2,self%nedges) )
    call calc_cell_on_edge( self%edge_on_cell_2d, &
                            self%nedges_per_cell, &
                            self%ncells,          &
                            self%cell_on_edge_2d, &
                            self%nedges,          &
                            self%void_cell )

    ! Allocate each vertex to the cell with the highest global cell index
    ! of the cells neighbouring the vertex.
    allocate( self%vert_cell_owner(self%nverts) )
    do ientity=1,self%nverts
      self%vert_cell_owner(ientity)=maxval( self%cell_on_vert_2d(:,ientity) )
    end do

    ! Allocate each edge to the cell with the highest global cell index
    ! of the cells neighbouring the edge.
    allocate( self%edge_cell_owner(self%nedges) )
    do ientity=1,self%nedges
      self%edge_cell_owner(ientity)=maxval( self%cell_on_edge_2d(:,ientity) )
    end do

    ! Initialise values in this objects global mesh maps collection.
    if (.not. allocated(self%global_mesh_maps) ) &
        allocate ( self%global_mesh_maps,        &
                    source = global_mesh_map_collection_type() )

  end function global_mesh_constructor

  !===========================================================================
  !> @brief Constructs a small example mesh for unit testing purposes.
  !>
  !> @return 2D global mesh object based on a 9-cell global mesh. 3x3 cell
  !>         arrangement of quadrilateral cells.
  !>
  function global_mesh_constructor_unit_test_data() result (self)

    implicit none

    type(global_mesh_type) :: self

    integer(i_def), parameter :: void = -9999

    global_mesh_id_counter = global_mesh_id_counter + 1

    call self%set_id(global_mesh_id_counter)

    ! The feigner in the mesh unit test sets geometry to spherical,
    ! so make the global mesh used in the mesh unit tests consistent.
    self%geometry  = spherical_domain
    self%topology  = non_periodic_domain
    self%coord_sys = lon_lat_coords
    self%mesh_name = 'planar:unit-test'

    self%nverts_per_cell = 4
    self%nedges_per_cell = 4
    self%max_cells_per_vertex = 4

    ! Returns global_mesh_object of size 3x3 quad reference cell.
    ! As per reference cell, direction of numbering is anti-clockwise.
    ! Starting point is
    ! Vertices: Bottom Left (south-west)
    ! Edges:    Left        (west)
    ! Faces:    Left        (west)
    self%ncells = 9
    self%nverts = 16
    self%nedges = 24

    allocate( self%cell_next_2d    (self%nedges_per_cell, self%ncells) )
    allocate( self%vert_on_cell_2d (self%nverts_per_cell, self%ncells) )
    allocate( self%edge_on_cell_2d (self%nedges_per_cell, self%ncells) )
    allocate( self%cell_on_vert_2d (self%max_cells_per_vertex, self%nverts) )
    allocate( self%cell_on_edge_2d (2, self%nedges) )
    allocate( self%vert_on_edge_2d (2, self%nedges) )
    allocate( self%vert_coords     (2, self%nverts) )
    allocate( self%cell_coords     (2, self%ncells) )
    allocate( self%vert_cell_owner (self%nverts) )
    allocate( self%edge_cell_owner (self%nedges) )

    self%void_cell = void
    self%ntarget_meshes = 0
    self%domain_extents(:,1) = [ -2.0_r_def, -2.0_r_def ]
    self%domain_extents(:,2) = [  2.0_r_def, -2.0_r_def ]
    self%domain_extents(:,3) = [  2.0_r_def,  2.0_r_def ]
    self%domain_extents(:,4) = [ -2.0_r_def,  2.0_r_def ]

    ! Note: These test coordinates are in [Long, Lat] in units of radians.
    self%coord_units_xy(:)   = 'radians'
    self%vert_coords(1:2,1)  = [-2.0_r_def, -2.0_r_def]
    self%vert_coords(1:2,2)  = [-1.0_r_def, -2.0_r_def]
    self%vert_coords(1:2,3)  = [ 1.0_r_def, -2.0_r_def]
    self%vert_coords(1:2,4)  = [ 2.0_r_def, -2.0_r_def]
    self%vert_coords(1:2,5)  = [-2.0_r_def, -1.0_r_def]
    self%vert_coords(1:2,6)  = [-1.0_r_def, -1.0_r_def]
    self%vert_coords(1:2,7)  = [ 1.0_r_def, -1.0_r_def]
    self%vert_coords(1:2,8)  = [ 2.0_r_def, -1.0_r_def]
    self%vert_coords(1:2,9)  = [-2.0_r_def,  1.0_r_def]
    self%vert_coords(1:2,10) = [-1.0_r_def,  1.0_r_def]
    self%vert_coords(1:2,11) = [ 1.0_r_def,  1.0_r_def]
    self%vert_coords(1:2,12) = [ 2.0_r_def,  1.0_r_def]
    self%vert_coords(1:2,13) = [-2.0_r_def,  2.0_r_def]
    self%vert_coords(1:2,14) = [-1.0_r_def,  2.0_r_def]
    self%vert_coords(1:2,15) = [ 1.0_r_def,  2.0_r_def]
    self%vert_coords(1:2,16) = [ 2.0_r_def,  2.0_r_def]

    ! Note: These test coordinates are in [Long, Lat] in units of radians.
    self%cell_coords(1:2,1)  = [-1.5_r_def, -1.5_r_def]
    self%cell_coords(1:2,2)  = [ 0.0_r_def, -1.5_r_def]
    self%cell_coords(1:2,3)  = [ 1.5_r_def, -1.5_r_def]
    self%cell_coords(1:2,4)  = [-1.5_r_def,  0.0_r_def]
    self%cell_coords(1:2,5)  = [ 0.0_r_def,  0.0_r_def]
    self%cell_coords(1:2,6)  = [ 1.5_r_def,  0.0_r_def]
    self%cell_coords(1:2,7)  = [-1.5_r_def,  1.5_r_def]
    self%cell_coords(1:2,8)  = [ 0.0_r_def,  1.5_r_def]
    self%cell_coords(1:2,9)  = [ 1.5_r_def,  1.5_r_def]

    self%cell_next_2d(:,1)     = [void, void,    2,    4]
    self%cell_next_2d(:,2)     = [   1, void,    3,    5]
    self%cell_next_2d(:,3)     = [   2, void, void,    6]
    self%cell_next_2d(:,4)     = [void,    1,    5,    7]
    self%cell_next_2d(:,5)     = [   4,    2,    6,    8]
    self%cell_next_2d(:,6)     = [   5,    3, void,    9]
    self%cell_next_2d(:,7)     = [void,    4,    8, void]
    self%cell_next_2d(:,8)     = [   7,    5,    9, void]
    self%cell_next_2d(:,9)     = [   8,    6, void, void]

    self%vert_on_cell_2d(:,1)  = [ 1,  2,  6,  5]
    self%vert_on_cell_2d(:,2)  = [ 2,  3,  7,  6]
    self%vert_on_cell_2d(:,3)  = [ 3,  4,  8,  7]
    self%vert_on_cell_2d(:,4)  = [ 5,  6, 10,  9]
    self%vert_on_cell_2d(:,5)  = [ 6,  7, 11, 10]
    self%vert_on_cell_2d(:,6)  = [ 7,  8, 12, 11]
    self%vert_on_cell_2d(:,7)  = [ 9, 10, 14, 13]
    self%vert_on_cell_2d(:,8)  = [10, 11, 15, 14]
    self%vert_on_cell_2d(:,9)  = [11, 12, 16, 15]

    self%edge_on_cell_2d(:,1)  = [4,   1,  5,  8]
    self%edge_on_cell_2d(:,2)  = [5,   2,  6,  9]
    self%edge_on_cell_2d(:,3)  = [6,   3,  7, 10]
    self%edge_on_cell_2d(:,4)  = [11,  8, 12, 15]
    self%edge_on_cell_2d(:,5)  = [12,  9, 13, 16]
    self%edge_on_cell_2d(:,6)  = [13, 10, 14, 17]
    self%edge_on_cell_2d(:,7)  = [18, 15, 19, 22]
    self%edge_on_cell_2d(:,8)  = [19, 16, 20, 23]
    self%edge_on_cell_2d(:,9)  = [20, 17, 21, 24]

    self%cell_on_vert_2d(:,1)  = [void, void,    1, void]
    self%cell_on_vert_2d(:,2)  = [void, void,    2,    1]
    self%cell_on_vert_2d(:,3)  = [void, void,    3,    2]
    self%cell_on_vert_2d(:,4)  = [void, void, void,    3]
    self%cell_on_vert_2d(:,5)  = [void,    1,    4, void]
    self%cell_on_vert_2d(:,6)  = [   1,    2,    5,    4]
    self%cell_on_vert_2d(:,7)  = [   2,    3,    6,    5]
    self%cell_on_vert_2d(:,8)  = [   3, void, void,    6]
    self%cell_on_vert_2d(:,9)  = [void,    4,    7, void]
    self%cell_on_vert_2d(:,10) = [   4,    5,    8,    7]
    self%cell_on_vert_2d(:,11) = [   5,    6,    9,    8]
    self%cell_on_vert_2d(:,12) = [   6, void, void,    9]
    self%cell_on_vert_2d(:,13) = [void,    7, void, void]
    self%cell_on_vert_2d(:,14) = [   7,    8, void, void]
    self%cell_on_vert_2d(:,15) = [   8,    9, void, void]
    self%cell_on_vert_2d(:,16) = [   9, void, void, void]

    self%cell_on_edge_2d(:,1)  = [void,    1]
    self%cell_on_edge_2d(:,2)  = [void,    2]
    self%cell_on_edge_2d(:,3)  = [void,    3]
    self%cell_on_edge_2d(:,4)  = [   1, void]
    self%cell_on_edge_2d(:,5)  = [   2,    1]
    self%cell_on_edge_2d(:,6)  = [   3,    2]
    self%cell_on_edge_2d(:,7)  = [void,    3]
    self%cell_on_edge_2d(:,8)  = [   1,    4]
    self%cell_on_edge_2d(:,9)  = [   2,    5]
    self%cell_on_edge_2d(:,10) = [   3,    6]
    self%cell_on_edge_2d(:,11) = [   4, void]
    self%cell_on_edge_2d(:,12) = [   5,    4]
    self%cell_on_edge_2d(:,13) = [   6,    5]
    self%cell_on_edge_2d(:,14) = [void,    6]
    self%cell_on_edge_2d(:,15) = [   4,    7]
    self%cell_on_edge_2d(:,16) = [   5,    8]
    self%cell_on_edge_2d(:,17) = [   6,    9]
    self%cell_on_edge_2d(:,18) = [   7, void]
    self%cell_on_edge_2d(:,19) = [   8,    7]
    self%cell_on_edge_2d(:,20) = [   9,    8]
    self%cell_on_edge_2d(:,21) = [void,    9]
    self%cell_on_edge_2d(:,22) = [   7, void]
    self%cell_on_edge_2d(:,23) = [   8, void]
    self%cell_on_edge_2d(:,24) = [   9, void]

    self%vert_on_edge_2d(:,1)   = [1, 2]
    self%vert_on_edge_2d(:,2)   = [2, 3]
    self%vert_on_edge_2d(:,3)   = [3, 4]
    self%vert_on_edge_2d(:,4)   = [1, 5]
    self%vert_on_edge_2d(:,5)   = [2, 6]
    self%vert_on_edge_2d(:,6)   = [3, 7]
    self%vert_on_edge_2d(:,7)   = [4, 8]
    self%vert_on_edge_2d(:,8)   = [5, 6]
    self%vert_on_edge_2d(:,9)   = [6, 7]
    self%vert_on_edge_2d(:,10)  = [7, 8]
    self%vert_on_edge_2d(:,11)  = [5, 9]
    self%vert_on_edge_2d(:,12)  = [6,10]
    self%vert_on_edge_2d(:,13)  = [7,11]
    self%vert_on_edge_2d(:,14)  = [8,12]
    self%vert_on_edge_2d(:,15)  = [9,10]
    self%vert_on_edge_2d(:,16)  = [10,11]
    self%vert_on_edge_2d(:,17)  = [11,12]
    self%vert_on_edge_2d(:,18)  = [9, 13]
    self%vert_on_edge_2d(:,19)  = [10,14]
    self%vert_on_edge_2d(:,20)  = [11,15]
    self%vert_on_edge_2d(:,21)  = [12,16]
    self%vert_on_edge_2d(:,22)  = [13,14]
    self%vert_on_edge_2d(:,23)  = [14,15]
    self%vert_on_edge_2d(:,24)  = [15,16]

    self%vert_cell_owner(:) = [1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 7, 8, 9, 9]
    self%edge_cell_owner(:) = [1, 2, 3, 1, 2, 3, 3, 4, 5, 6, 4, 5, 6, 6, 7, 8, &
                               9, 7, 8, 9, 9, 7, 8, 9]

    ! Initialise values in this objects global mesh maps collection.
    if (.not. allocated(self%global_mesh_maps) ) then
      allocate( self%global_mesh_maps, &
                source=global_mesh_map_collection_type() )
    end if

  end function global_mesh_constructor_unit_test_data

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Destroys a global_mesh object when it is finished with.
  !>
  subroutine global_mesh_destructor(self)

    implicit none

    type (global_mesh_type), intent(inout) :: self

    !> @todo Is there a reason why this is a separate function? It seems
    !>       like a global mesh should be immutable.
    call self%clear()

  end subroutine global_mesh_destructor

  !-------------------------------------------------------------------------------
  ! Returns the cells on vertices. PRIVATE subroutine.
  !-------------------------------------------------------------------------------
  ! Details: Calculates the cells that are incident on a vertex by looping through
  !          the vertices on all cells (which we store) and filling an array based
  !          on vertex number with the cells around it.
  ! Input:   vert_on_cell    Array with indices of vertices on cells.
  !          verts_per_cell  Number of vertices per cell.
  !          ncell           Number of cells.
  !          cells_per_vert  Number of cells per vertex.
  !          nvert           Number of vertices.
  !          void_cell       Cell ID to use for null connectivity.
  ! Output:  cell_on_vert    Array with indices of cells on vertices.
  !-------------------------------------------------------------------------------
  subroutine calc_cell_on_vertex( vert_on_cell,   &
                                  verts_per_cell, &
                                  ncell,          &
                                  cell_on_vert,   &
                                  cells_per_vert, &
                                  nvert,          &
                                  void_cell )

  implicit none

  integer(i_def), intent(in)  :: verts_per_cell, ncell
  integer(i_def), intent(in)  :: vert_on_cell(verts_per_cell, ncell)
  integer(i_def), intent(in)  :: cells_per_vert, nvert, void_cell
  integer(i_def), intent(out) :: cell_on_vert(cells_per_vert, nvert)

  integer(i_def) :: cell
  integer(i_def) :: vertno
  integer(i_def) :: cellno
  integer(i_def) :: vert

  cell_on_vert = void_cell

  ! There is no order to how the cell IDs are listed around the vertex
  ! Some may have 4 or 3 (i.e. vertices on corners of panels).
  do cell=1, ncell
    do vertno=1, verts_per_cell

      vert = vert_on_cell(vertno,cell)

      do cellno=1, cells_per_vert
        if (cell_on_vert(cellno,vert) == cell) exit
        if (cell_on_vert(cellno,vert) == void_cell) then
          cell_on_vert(cellno,vert) = cell
          exit
        end if
      end do

    end do
  end do

  end subroutine calc_cell_on_vertex

  !-------------------------------------------------------------------------------
  ! Returns the cells on edges. PRIVATE subroutine.
  !-------------------------------------------------------------------------------
  ! Details: Calculates the cells that are either side of an edge by looping
  !          through the edges on all cells (which we store) and filling an array
  !          based on edge number with the cells around it.
  ! Input:   edge_on_cell    Array with indices of edges on cells.
  !          edges_per_cell  Number of edges per cell.
  !          ncell           Number of cells.
  !          nedge           Number of edges.
  !          void_cell       Cell ID to use for null connectivity.
  ! Output:  cell_on_edge    Array with indices of cells on edges.
  !-------------------------------------------------------------------------------
  subroutine calc_cell_on_edge( edge_on_cell,   &
                                edges_per_cell, &
                                ncell,          &
                                cell_on_edge,   &
                                nedge,          &
                                void_cell )
  implicit none

  integer(i_def), intent(in)  :: edges_per_cell, ncell
  integer(i_def), intent(in)  :: edge_on_cell(edges_per_cell, ncell)
  integer(i_def), intent(in)  :: nedge, void_cell
  integer(i_def), intent(out) :: cell_on_edge(2, nedge)

  integer(i_def) :: cell
  integer(i_def) :: edgeno
  integer(i_def) :: cellno
  integer(i_def) :: edge

  cell_on_edge = void_cell

  do cell=1,ncell
    do edgeno=1,edges_per_cell

      edge=edge_on_cell(edgeno,cell)

      do cellno=1, 2
        if ( cell_on_edge(cellno,edge) == cell ) exit
        if ( cell_on_edge(cellno,edge) == void_cell ) then
          cell_on_edge(cellno,edge)=cell
          exit
        end if
      end do
    end do
  end do

  end subroutine calc_cell_on_edge


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Returns the number of panels in the mesh.
  !>
  !> @details Returns the number of panels in the mesh -
  !>          cubedsphere = 6,
  !>          planar = 1
  !>
  !> @return npanels The number of panels in the mesh.
  !>
  function get_npanels( self ) result ( npanels )

    implicit none

    class(global_mesh_type), intent(in) :: self

    integer(i_def) :: npanels

    npanels = self%npanels

  end function get_npanels


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Queries the ID value used for void cells. These are invalid cells
  !>         in the cell-cell connectivity.
  !> @return cell_id  The ID value used for null cell-connectivity.
  !>
  function get_void_cell( self ) result ( cell_id )

    implicit none

    class(global_mesh_type), intent(in) :: self

    integer(i_def) :: cell_id

    cell_id = self%void_cell

  end function get_void_cell


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Queries if the global mesh domain geometry is spherical.
  !> @return answer .true. for a spherical domain surface.
  !>
  function is_geometry_spherical( self ) result ( answer )

    implicit none

    class(global_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%geometry == spherical_domain )

  end function is_geometry_spherical


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Queries if the global mesh domain geometry is a flat surface.
  !> @return answer .true. for a flat domain surface.
  !>
  function is_geometry_planar( self ) result ( answer )

    implicit none

    class(global_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%geometry == planar_domain )

  end function is_geometry_planar


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Queries if the global mesh topology specifies a domain where all the
  !>         boundaries are closed.
  !> @return answer .true. for a domain which is non-periodic.
  !>
  function is_topology_non_periodic( self ) result ( answer )

    implicit none

    class(global_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%topology == non_periodic_domain )

  end function is_topology_non_periodic


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Queries if the global mesh topology specifies a domain where
  !>         there is a single pair of periodic boundaries.
  !> @return answer .true. for a channel domain.
  !>
  function is_topology_channel( self ) result ( answer )

    implicit none

    class(global_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%topology == channel_domain )

  end function is_topology_channel


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Queries if the global mesh topology specifies a domain where all
  !>         domain boundaries are open with periodicity.
  !> @return answer .true. for a fully periodic domain.
  !>
  function is_topology_periodic( self ) result ( answer )

    implicit none

    class(global_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%topology == periodic_domain )

  end function is_topology_periodic


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Queries if the global mesh nodes are specified using Cartesian
  !>         co-ordinates (x,y,z).
  !> @return answer .true. if nodes are specified in Cartesian co-ordinates.
  !>
  function is_coord_sys_xyz( self ) result ( answer )

    implicit none

    class(global_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%coord_sys == xyz_coords )

  end function is_coord_sys_xyz

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Queries if the global mesh nodes are specified using Spherical
  !>         co-ordinates (longitude, latitude).
  !> @return answer .true. if nodes are specified in spherical co-ordinates.
  !>
  function is_coord_sys_ll( self ) result ( answer )

    implicit none

    class(global_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%coord_sys == lon_lat_coords )

  end function is_coord_sys_ll


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Returns mesh tag name.
  !> @return mesh_name  Tag name of mesh that identifies it in the
  !>                    UGRID file that it was read in from.
  !>
  function get_mesh_name( self ) result ( mesh_name )

    implicit none

    class(global_mesh_type), intent(in) :: self

    character(str_def) :: mesh_name

    mesh_name = self%mesh_name

  end function get_mesh_name


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief   Returns values for the X and Y periodicity.
  !>
  !> @details periodic_xy as read from the mesh file are returned.
  !>          This functions is currently only used during the
  !>          partitioning of planar meshes for determining mesh extents.
  !>          The values are obtained from the header of the mesh files
  !>          and are set to "true" for cubedsphere meshes (due to mesh
  !>          IO function requirements) and to a user defined value for
  !>          planar meshes.
  !>
  !> @return periodic_xy  Mesh domain periodicity in x/y-axes.
  !>
  function get_mesh_periodicity( self ) result ( periodic_xy )
    implicit none

    class(global_mesh_type), intent(in) :: self
    logical(l_def) :: periodic_xy(2)


    periodic_xy = self%periodic_xy

  end function get_mesh_periodicity


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief   Returns the input arguments to constructor.
  !> @details The returned string records the arguments used to generate the
  !>          global mesh for output to mesh input files. Principally so
  !>          meshes can be regenerated at later code revisions if a user only
  !>          possesses the input mesh file.
  !>
  !> @return  string   A string listing the arguments that were passed to the
  !>                   global mesh constructor function.
  !>
  function get_constructor_inputs( self ) result ( constructor_inputs )

    implicit none

    class(global_mesh_type), intent(in) :: self
    character(str_longlong)             :: constructor_inputs

    constructor_inputs = self%constructor_inputs

  end function get_constructor_inputs


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

    class(global_mesh_type), intent(in) :: self
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

    class(global_mesh_type), intent(in) :: self
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

    class(global_mesh_type), intent(in) :: self
    real(r_def) :: equatorial_latitude

    equatorial_latitude = self%equatorial_latitude

  end function get_equatorial_latitude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Returns ID of a cell in this mesh.
  !>
  !> @details Gets the cell ID that is x_cells across (E is +ve) and y_cells
  !>          up/down (N is +ve) from given cell_number.
  !>
  !> @note For a cubed -sphere mesh, this will only return correct cell IDs if
  !>       the offset cell remains on same the cubed-sphere "face" as the
  !>       start cell.
  !>
  !> @param[in] cell_number ID of the anchor element.
  !> @param[in] x_cells Offset in the E/W direction.
  !> @param[in] y_cells Offset in the N/S direction.
  !>
  !> @return cell_id ID of the cell at the given offset to the start cell.
  !>
  function get_cell_id( self, cell_number, &
                        x_cells, y_cells ) result ( cell_id )

  use reference_element_mod, only : W, S, E, N

  implicit none

  class(global_mesh_type), intent(in) :: self

  integer(i_def), intent(in) :: cell_number
  integer(i_def), intent(in) :: x_cells, y_cells

  integer(i_def) :: cell_id

  integer(i_def) :: index, dist, i

  cell_id=cell_number

  if (x_cells > 0 )then
    index = E
    dist = x_cells
  else if (x_cells < 0 )then
    index = W
    dist = abs(x_cells)
  else
    index = W
    dist = 0
  endif
  do i = 1,dist
    cell_id = self%cell_next_2d(index,cell_id)
  end do

  if (y_cells > 0 )then
    index = N
    dist = y_cells
  else if (y_cells < 0 )then
    index = S
    dist = abs(y_cells)
  else
    index = S
    dist = 0
  endif
  do i = 1,dist
    cell_id = self%cell_next_2d(index,cell_id)
  end do

  end function get_cell_id

  !---------------------------------------------------------------------------
  !> @brief Gets the cells that are incident on a particular vertex.
  !>
  !> @param[in] vertex_number Number of the vertex being queried.
  !> @param[out] cells Cells around the given vertex.
  !>
  subroutine get_cell_on_vert( self, vertex_number, cells)

  implicit none

  class(global_mesh_type), intent(in) :: self

  integer(i_def), intent(in)  :: vertex_number
  integer(i_def), intent(out) :: cells(:)

  cells = self%cell_on_vert_2d(:,vertex_number)

  end subroutine get_cell_on_vert

  !---------------------------------------------------------------------------
  !> @brief Gets the cells that are incident on a particular edge.
  !>
  !> @param[in]  edge_number  Number of the edge being queried.
  !> @param[out] cells        Cells either side of the given edge.
  !>
  subroutine get_cell_on_edge( self, edge_number, cells)

  implicit none

  class(global_mesh_type), intent(in) :: self
  integer(i_def), intent(in)  :: edge_number
  integer(i_def), intent(out) :: cells(:)

  cells=self%cell_on_edge_2d(:,edge_number)

  end subroutine get_cell_on_edge

  !---------------------------------------------------------------------------
  !> @brief Gets the total number of vertices in the global domain.
  !>
  !> @return Total number of vertices in the global domain.
  !>
  function get_nverts( self ) result (nverts)

  implicit none

  class(global_mesh_type), intent(in) :: self

  integer(i_def) :: nverts

  nverts = self%nverts

  end function get_nverts

  !---------------------------------------------------------------------------
  !> @brief Returns the number of intergrid maps that exist for this mesh
  !>        where this mesh is the source mesh.
  !>
  !> @return Number of intergrid maps present in the UGRID file which use
  !>         this mesh as the source mesh.
  !>
  function get_nmaps( self ) result (nmaps)

  implicit none

  class(global_mesh_type), intent(in) :: self

  integer(i_def) :: nmaps

  nmaps = self%ntarget_meshes

  end function get_nmaps

  !---------------------------------------------------------------------------
  !> @brief Returns the global mesh map collection assigned to this
  !>        global mesh, were this mesh is treated as the source
  !>        mesh in the maps contained.
  !>
  !> @return global_maps  Mesh map collection <<pointer>>.
  !>
  function get_mesh_maps( self) result( global_maps  )

    implicit none

    class (global_mesh_type), intent(in), target :: self

    type(global_mesh_map_collection_type), pointer :: global_maps

    nullify(global_maps)
    global_maps => self%global_mesh_maps

  end function  get_mesh_maps


  !---------------------------------------------------------------------------
  !> @brief   Gets the array of names of target meshes.
  !> @details The global mesh may have intergrid maps contained within the
  !>          mesh inputs file. These maps provide a cell mapping from this
  !>          mesh (source) to another mesh (target) in the mesh inputs file.
  !>          The returned array of mesh names are the names of the meshes
  !>          that this global mesh has maps to.
  !>
  !> @param[out] target_mesh_names  String array of target mesh topology names.
  !>
  subroutine get_target_mesh_names( self, target_mesh_names)

    implicit none

    class(global_mesh_type), intent(in) :: self

    character(str_def), intent(out), &
                            allocatable :: target_mesh_names(:)

    if (self%ntarget_meshes > 0) then
      allocate( target_mesh_names(self%ntarget_meshes) )
      target_mesh_names = self%target_global_mesh_names
    else

    end if

  end subroutine get_target_mesh_names

  !---------------------------------------------------------------------------
  !> @brief Gets the total number of edges in the global domain.
  !>
  !> @return nedges  The total number of edges in the global domain.
  !>
  function get_nedges( self ) result (nedges)

  implicit none

  class(global_mesh_type), intent(in) :: self

  integer(i_def) :: nedges

  nedges = self%nedges

  end function get_nedges

  !---------------------------------------------------------------------------
  !> @brief Gets the total number of cells in the global domain.
  !>
  !> @return ncells  The total number of cells in the global domain.
  !>
  function get_ncells( self ) result (ncells)

  implicit none

  class(global_mesh_type), intent(in) :: self

  integer(i_def) :: ncells

  ncells = self%ncells

  end function get_ncells

  !---------------------------------------------------------------------------
  !> @brief   Gets the maximum number of cells around a vertex.
  !> @details The actual number of cells at a particular vertex could be
  !>          fewer than the value returned by this method.
  !>
  !>          For example, in a cubed sphere mesh, there are generally
  !>          four cells incident with a vertex except for the "corner"
  !>          vertices where there are three.
  !>
  !> @return  Maximum number of cells that can be incident with a vertex.
  !>
  function get_max_cells_per_vertex( self ) result (max_cells_per_vertex)

  implicit none

  class(global_mesh_type), intent(in) :: self

  integer(i_def) :: max_cells_per_vertex

  max_cells_per_vertex = self%max_cells_per_vertex

  end function get_max_cells_per_vertex

  !---------------------------------------------------------------------------
  !> @brief Gets the edges that are incident with a particular cell.
  !>
  !> @param[in]  cell_gid  Global ID of the cell being queried.
  !> @param[out] edges     Edge global IDs around the given cell.
  !>
  subroutine get_edge_on_cell(self, cell_gid, edges)

    implicit none
    class (global_mesh_type), intent(in)  :: self
    integer (i_def),          intent(in)  :: cell_gid
    integer (i_def),          intent(out) :: edges(:)

    edges(:) = self%edge_on_cell_2d(:,cell_gid)

  end subroutine get_edge_on_cell

  !---------------------------------------------------------------------------
  !> @brief Returns entire array for edges on 2d cells.
  !> @return edges [Edge,Cell] connectivity array in global IDs.
  !>
  function get_edge_on_all_cells(self) result(edges)

    implicit none

    class(global_mesh_type), intent(in)  :: self

    integer(i_def), allocatable :: edges(:,:)

    edges = self%edge_on_cell_2d(:,:)

  end function get_edge_on_all_cells

  !---------------------------------------------------------------------------
  !> @brief Gets the vertices that are incident with a particular cell.
  !>
  !> @param[in]  cell_gid  Global ID of the cell being queried.
  !> @param[out] verts     Vertex global IDs  around the given cell.
  !>
  subroutine get_vert_on_cell(self, cell_gid, verts)

    implicit none

    class (global_mesh_type), intent(in)  :: self
    integer (i_def),          intent(in)  :: cell_gid
    integer (i_def),          intent(out) :: verts(:)

    verts(:) = self%vert_on_cell_2d(:,cell_gid)

  end subroutine get_vert_on_cell

  !---------------------------------------------------------------------------
  !> @brief Gets the vertices that form endpoints of a given edge.
  !> @details Global IDs of vertices which construct a given edge. No information
  !>          should be inferred by the order in which the vertices are presented.
  !> @param[in] edge_gid Global ID of the edge being queried.
  !> @return integer[2] Vertex global IDs.
  !>
  function get_vert_on_edge( self, edge_gid ) result( verts )

    implicit none
    class (global_mesh_type), intent(in)  :: self
    integer (i_def),          intent(in)  :: edge_gid
    integer (i_def)                       :: verts(2)

    verts(:) = self%vert_on_edge_2d(:, edge_gid)

  end function get_vert_on_edge

  !---------------------------------------------------------------------------
  !> @brief Returns entire array for vertices on 2d cells.
  !> @return verts [vert,cell] connectivity array in global IDs
  !>
  function get_vert_on_all_cells(self) result(verts)

    implicit none

    class (global_mesh_type), intent(in)  :: self

    integer(i_def), allocatable :: verts(:,:)

    verts = self%vert_on_cell_2d(:,:)

  end function get_vert_on_all_cells

  !---------------------------------------------------------------------------
  !> @brief Gets the number of vertices per 2D-cell.
  !>
  !> @return Number of vertices per 2D-cell.
  !>
  function get_nverts_per_cell( self ) result (nverts_per_cell)

    implicit none
    class(global_mesh_type), intent(in) :: self
    integer(i_def)                      :: nverts_per_cell

    nverts_per_cell = self%nverts_per_cell

  end function get_nverts_per_cell

  !---------------------------------------------------------------------------
  !> @brief Gets the number of vertices per 2D-edge.
  !>
  !> @return Number of vertices per 2D-edge.
  !>
  function get_nverts_per_edge( self ) result (nverts_per_edge)

    implicit none
    class(global_mesh_type), intent(in) :: self
    integer(i_def)                      :: nverts_per_edge

    nverts_per_edge = self%nverts_per_edge

  end function get_nverts_per_edge


  !---------------------------------------------------------------------------
  !> @brief Gets the number of edges on each cell.
  !>
  !> @return Number of edges per 2D-cell.
  !>
  function get_nedges_per_cell( self ) result (nedges_per_cell)

    implicit none
    class(global_mesh_type), intent(in) :: self
    integer(i_def)                      :: nedges_per_cell

    nedges_per_cell = self%nedges_per_cell

  end function get_nedges_per_cell

  !---------------------------------------------------------------------------
  !> @brief Gets cell IDs adjacent to a cell.
  !>
  !> @param[in]  cell_gid   Global ID of single cell.
  !> @param[out] cell_next  Global IDs of cells adjacent to cell cell_gid.
  !>
  subroutine get_cell_next (self, cell_gid, cell_next)

    implicit none
    class (global_mesh_type), intent(in)  :: self
    integer (i_def),          intent(in)  :: cell_gid
    integer (i_def),          intent(out) :: cell_next(:)

    cell_next(:) = self%cell_next_2d(:,cell_gid)

  end subroutine get_cell_next

  !---------------------------------------------------------------------------
  !> @brief Returns entire cell_next connectivity array.
  !> @return cell_next [cell_next,cell] connectivity array in global IDs.
  !>
  function get_all_cells_next (self) result(cell_next)

    implicit none

    class(global_mesh_type), intent(in)  :: self

    integer(i_def), allocatable :: cell_next(:,:)

    cell_next = self%cell_next_2d(:,:)

  end function get_all_cells_next

  !---------------------------------------------------------------------------
  !> @brief Gets vertex coordinates.
  !>
  !> @param[in] vert_gid Global ID of a vertex.
  !> @param[out] vert_coords Latitude and longitude of the specified vertex.
  !>
  subroutine get_a_vert_coords (self, vert_gid, vert_coords)

    implicit none
    class (global_mesh_type), intent(in)  :: self
    integer(i_def),           intent(in)  :: vert_gid
    real(r_def),              intent(out) :: vert_coords(:)

    vert_coords(1:2) = self%vert_coords(1:2,vert_gid)

  end subroutine get_a_vert_coords

  !---------------------------------------------------------------------------
  !> @brief Gets coordinates for all unique nodes on mesh.
  !>
  !> @param[out] vert_coords X/Y axes coordinates of all unique vertices.
  !>
  subroutine get_all_vert_coords (self, vert_coords)

    implicit none
    class (global_mesh_type), intent(in)  :: self
    real(r_def), allocatable :: vert_coords(:,:)

    if ( allocated(vert_coords) ) deallocate (vert_coords)
    allocate( vert_coords, source=self%vert_coords )

  end subroutine get_all_vert_coords

  !---------------------------------------------------------------------------
  !> @brief Gets the coordinates of the centre of one single cell.
  !>
  !> @param[in] cell_gid Global ID of a cell.
  !> @param[out] cell_coords Latitude and longitude of the specified cell.
  !>
  subroutine get_a_cell_coords (self, cell_gid, cell_coords)

    implicit none
    class (global_mesh_type), intent(in)  :: self
    integer(i_def),           intent(in)  :: cell_gid
    real(r_def),              intent(out) :: cell_coords(:)

    cell_coords(1:2) = self%cell_coords(1:2,cell_gid)

  end subroutine get_a_cell_coords

  !---------------------------------------------------------------------------
  !> @brief Gets coordinates for the centres of all cells
  !>
  !> @param[out] cell_coords Latitude and longitude of the centre of all cells
  !>
  subroutine get_all_cell_coords (self, cell_coords)

    implicit none
    class (global_mesh_type), intent(in)  :: self
    real(r_def), allocatable :: cell_coords(:,:)

    if ( allocated(cell_coords) ) deallocate (cell_coords)
    allocate( cell_coords, source=self%cell_coords )

  end subroutine get_all_cell_coords


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the cell that a vertex has been allocated to.
  !>
  !> @param[in] vert Global ID of the vertex.
  !>
  !> @return Global cell ID that the vertex has been allocated to.
  !>
  function get_vert_cell_owner ( self, vert ) result ( cell )

    implicit none
    class (global_mesh_type), intent(in)  :: self
    integer (i_def),          intent(in)  :: vert
    integer (i_def)                       :: cell

    cell = self%vert_cell_owner( vert )

  end function get_vert_cell_owner

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the cell that an edge has been allocated to.
  !>
  !> @param [in] edge_gid Global ID of the edge.
  !>
  !> @return Global cell ID that the edge has been allocated to.
  !>
  function get_edge_cell_owner ( self, edge ) result ( cell )

    implicit none
    class (global_mesh_type), intent(in)  :: self
    integer (i_def),          intent(in)  :: edge
    integer (i_def)                       :: cell

    cell = self%edge_cell_owner( edge )

  end function get_edge_cell_owner

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Adds a global mesh map to from this global mesh (source) to
  !>        another global mesh (target).
  !> @param[in] target_global_mesh Target global mesh object to map to.
  !> @param[in] map Global ID map from source to target mesh with array
  !>             dimensions [ncells target cells per source cell,
  !>             ncells in source mesh].
  !>
  subroutine add_global_mesh_map(self, target_global_mesh, map)

    implicit none

    class(global_mesh_type), intent(inout)       :: self
    type(global_mesh_type),  intent(in), pointer :: target_global_mesh
    integer,                 intent(in)          :: map(:,:,:)

    integer(i_def) :: source_global_mesh_id
    integer(i_def) :: target_global_mesh_id

    source_global_mesh_id = self%get_id()
    target_global_mesh_id = target_global_mesh%get_id()

    ! Perform tests to see if this is a valid map to add.
    !==========================================================================
    if (source_global_mesh_id == target_global_mesh_id) then
      write(log_scratch_space, '(A)') &
          'Nothing to do, no need to map global mesh to itself.'
      call log_event(log_scratch_space, LOG_LEVEL_TRACE)
      return
    end if

    if (size(map,3) /= self%ncells) then
      write(log_scratch_space, '(A,I0,A,I0,A)')                      &
          'Invalid global mesh mapping: Number of source cells '   //&
          'in global mesh map (', size(map,3), ') does not match ' //&
          'number of source global mesh cells (', self%ncells, ')'
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
      return
    end if


    ! Ask the global mesh map collection to add this map to itself
    call self%global_mesh_maps%add_global_mesh_map( source_global_mesh_id, &
                                                    target_global_mesh_id, &
                                                    map )
    return

  end subroutine add_global_mesh_map

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the global mesh map which maps cells from this global mesh
  !>        (source) to the global mesh (target) with the specified global
  !>        mesh ID.
  !>
  !> @param[in] target_global_mesh_id ID of the target global mesh.
  !>
  !> @return Global mesh map object from this global mesh to a global
  !>         mesh with a specified global_mesh_id. A null pointer is
  !>         returned if the requested map object is unavailable.
  !>
  function get_global_mesh_map(self, target_global_mesh_id) &
                        result(global_mesh_map)

    implicit none

    class(global_mesh_type), intent(in) :: self
    integer,                 intent(in) :: target_global_mesh_id

    type(global_mesh_map_type), pointer :: global_mesh_map

    integer(i_def) :: source_global_mesh_id

    global_mesh_map => null()
    source_global_mesh_id = self%get_id()

    ! Ask the global mesh map collection to return the map
    ! from source->target.
    global_mesh_map =>                                                     &
        self%global_mesh_maps%get_global_mesh_map( source_global_mesh_id,  &
                                                  target_global_mesh_id )

    return

  end function get_global_mesh_map


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief   Returns the rim depth (in cells) of an LBC mesh.
  !> @details LBC meshes are paired with LAM meshes, their coverage is a ring
  !>          on the edge of the LAM domain. This function returns the depth
  !>          that this ring extendeds into the domain (in number of cells).
  !> @return  rim_depth  Number of cells from edge of domain.
  !>
  function get_rim_depth( self) result( rim_depth )

    implicit none

    class(global_mesh_type), intent(in) :: self
    integer(i_def) :: rim_depth

    rim_depth = self%rim_depth

  end function get_rim_depth


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief   Returns principal coordinates that describe the domain shape.
  !> @details The model domain extents of the global model. Units are in
  !>          line with geometry/coordinate system. This essentially defines
  !>          the range of valid coordinates, is does not imply these are the
  !>          range of coordinates of actual mesh nodes.
  !> @return  domain_extents  Principal coordinates that describe
  !>                          the domain shape.
  !>
  function get_domain_extents( self ) result( domain_extents )

    implicit none

    class(global_mesh_type), intent(in) :: self
    real(r_def)                         :: domain_extents(2,4)

    domain_extents(:,:) = self%domain_extents(:,:)

  end function get_domain_extents


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief   Returns the co-ordinate units of data in this mesh for x/y axes.
  !> @return  coord_units_xy Coordinate units along x/y-axes.
  !>
  function get_coord_units( self ) result( coord_units_xy )

    implicit none

    class(global_mesh_type), intent(in) ::  self
    character(str_def)                  :: coord_units_xy(2)

    coord_units_xy(:) = self%coord_units_xy(:)

  end function get_coord_units


  !---------------------------------------------------------------------------
  !> @brief Forced clear of all this oject from memory.
  !>
  !> @details Explicitly deallocates any allocatable arrays in the object to avoid
  !>          memory leaks. This routine should not need to be called manually
  !>          except in pfunit tests.
  !>
  subroutine clear(self)

    implicit none

    class (global_mesh_type), intent(inout) :: self

    if (allocated(self%vert_coords))              deallocate( self%vert_coords )
    if (allocated(self%cell_next_2d))             deallocate( self%cell_next_2d )
    if (allocated(self%vert_on_cell_2d))          deallocate( self%vert_on_cell_2d )
    if (allocated(self%vert_on_edge_2d))          deallocate( self%vert_on_edge_2d )
    if (allocated(self%cell_on_vert_2d))          deallocate( self%cell_on_vert_2d )
    if (allocated(self%edge_on_cell_2d))          deallocate( self%edge_on_cell_2d )
    if (allocated(self%cell_on_edge_2d))          deallocate( self%cell_on_edge_2d )
    if (allocated(self%vert_cell_owner))          deallocate( self%vert_cell_owner )
    if (allocated(self%edge_cell_owner))          deallocate( self%edge_cell_owner )
    if (allocated(self%target_global_mesh_names)) deallocate( self%target_global_mesh_names)
    if (allocated(self%global_mesh_maps))         call self%global_mesh_maps%clear()
    if (allocated(self%global_mesh_maps))         deallocate( self%global_mesh_maps )
    if (allocated(self%cell_coords))              deallocate( self%cell_coords )

    return
  end subroutine clear

end module global_mesh_mod
