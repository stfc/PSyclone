!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

!> @brief  Describes the cell ordering within the global mesh

!> @details This object holds the connectivities that fully
!>          describe the 2D topology of the global mesh

module global_mesh_mod

  use constants_mod,                  only: r_def, i_def, str_max_filename, &
                                            str_def, degrees_to_radians,    &
                                            l_def, i_native, emdi

  use global_mesh_map_mod,            only: global_mesh_map_type
  use global_mesh_map_collection_mod, only: global_mesh_map_collection_type
  use linked_list_data_mod,           only: linked_list_data_type
  use log_mod,                        only: log_event, log_scratch_space, &
                                            LOG_LEVEL_ERROR, LOG_LEVEL_TRACE

  implicit none

  private

  integer(i_native), parameter :: spherical_domain = 301
  integer(i_native), parameter :: planar_domain    = 302

  integer(i_native), parameter :: non_periodic_domain = 401
  integer(i_native), parameter :: channel_domain      = 402
  integer(i_native), parameter :: periodic_domain     = 403

  integer(i_native), parameter :: lon_lat_coords = 501
  integer(i_native), parameter :: xyz_coords     = 502


  type, extends(linked_list_data_type), public :: global_mesh_type

    private

  ! Tag name of mesh
    character(str_def) :: mesh_name
  ! Domain surface geometry
    integer(i_native)  :: geometry = emdi
  ! Domain boundaries topology
    integer(i_native)  :: topology = emdi
  ! Co-ordinate system used to specify node locations
    integer(i_native)  :: coord_sys = emdi
  ! Periodic in E-W direction
    logical(l_def)     :: periodic_x
  ! Periodic in N-S direction
    logical(l_def)     :: periodic_y
  ! Horizontal coords of vertices in full domain
    real(r_def), allocatable :: vert_coords(:,:)
  ! Horizontal coords of cells in full domain
    real(r_def), allocatable :: cell_coords(:,:)
  ! Full domain cell to cell connectivities
    integer(i_def), allocatable :: cell_next_2d(:,:)
  ! Full domain vertices on a cell
    integer(i_def), allocatable :: vert_on_cell_2d(:,:)
  ! Full domain cells that surround a vertex
    integer(i_def), allocatable :: cell_on_vert_2d(:,:)
  ! Full domain edges on a cell
    integer(i_def), allocatable :: edge_on_cell_2d(:,:)
  ! Full domain cells either side of an edge
    integer(i_def), allocatable :: cell_on_edge_2d(:,:)
  ! Full domain list the cells that vertices are allocated to
    integer(i_def), allocatable :: vert_cell_owner(:)
  ! Full domain list the cells that edges are allocated to
    integer(i_def), allocatable :: edge_cell_owner(:)
  ! Total number of vertices in the full domain
    integer(i_def)       :: nverts
  ! Total number of edges in the full domain
    integer(i_def)       :: nedges
  ! total number of cells in full domain
    integer(i_def)       :: ncells
  ! number of vertices on each cell
    integer(i_def)       :: nverts_per_cell
  ! number of vertices on each edge
    integer(i_def)       :: nverts_per_edge
  ! number of edges on each cell
    integer(i_def)       :: nedges_per_cell
  ! maximum number of cells around a vertex
    integer(i_def)       :: max_cells_per_vertex
  ! Number of panels on the mesh to be constructed
    integer(i_def)       :: npanels
  ! Target of global mesh name
    integer(i_def) :: ntarget_meshes
    character(str_def), allocatable :: target_global_mesh_names(:)
  ! Collection of global mesh maps in global cell ids
    type(global_mesh_map_collection_type), allocatable :: global_mesh_maps

  contains
    procedure, public :: get_mesh_name
    procedure, public :: get_npanels
    procedure, public :: get_mesh_periodicity
    procedure, public :: get_cell_id
    procedure, public :: get_cell_on_vert
    procedure, public :: get_cell_on_edge
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
    procedure, public :: get_vert_coords
    procedure, public :: get_vert_cell_owner
    procedure, public :: get_edge_cell_owner
    procedure, public :: add_global_mesh_map
    procedure, public :: get_global_mesh_map
    procedure, public :: get_target_mesh_names
    procedure, public :: get_nmaps

    procedure, public :: is_geometry_spherical
    procedure, public :: is_geometry_planar
    procedure, public :: is_topology_non_periodic
    procedure, public :: is_topology_channel
    procedure, public :: is_topology_periodic
    procedure, public :: is_coord_sys_xyz
    procedure, public :: is_coord_sys_ll

    procedure, public :: clear

    final :: global_mesh_destructor

  end type global_mesh_type

  interface global_mesh_type
    module procedure global_mesh_constructor
    module procedure global_mesh_constructor_unit_test_data
  end interface

  ! -------------------------------------------------------------------------
  ! Module parameters
  ! -------------------------------------------------------------------------

  ! Counter variable to keep track of the next mesh id number to uniquely
  ! identify each different mesh
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
  !> @param[in] npanels          Number of panels in the mesh
  !> @param[in] global_mesh_name Name of ugrid mesh topology to create
  !>                             global mesh object from.
  !>
  !> @return Freshly minted global_mesh_type object.
  !>
  function global_mesh_constructor( ugrid_mesh_data, &
                                    npanels ) result(self)

    use ugrid_mesh_data_mod, only: ugrid_mesh_data_type

    implicit none

    type(ugrid_mesh_data_type), intent(in) :: ugrid_mesh_data
    integer(i_def),             intent(in) :: npanels

    type(global_mesh_type) :: self

    character(str_def) :: geometry
    character(str_def) :: topology
    character(str_def) :: coord_sys

    ! loop counter over entities (vertices or edges)
    integer(i_def) :: ientity

    call ugrid_mesh_data%get_data( self%mesh_name, &
                                   geometry, &
                                   topology, &
                                   coord_sys, &
                                   self%nverts, &
                                   self%nedges, &
                                   self%ncells, &
                                   self%nverts_per_cell, &
                                   self%nverts_per_edge, &
                                   self%nedges_per_cell, &
                                   self%max_cells_per_vertex, &
                                   self%periodic_x, &
                                   self%periodic_y, &
                                   self%ntarget_meshes,  &
                                   self%target_global_mesh_names, &
                                   self%vert_coords, &
                                   self%cell_coords, &
                                   self%cell_next_2d, &
                                   self%vert_on_cell_2d, &
                                   self%edge_on_cell_2d )

    self%npanels = npanels

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
    ! code
    if (self%coord_sys == lon_lat_coords) then
      self%vert_coords(:,:) = self%vert_coords(:,:) * degrees_to_radians
      self%cell_coords(:,:) = self%cell_coords(:,:) * degrees_to_radians
    end if

    allocate( self%cell_on_vert_2d( self%max_cells_per_vertex, self%nverts ) )
    call calc_cell_on_vertex( self%vert_on_cell_2d, &
                              self%nverts_per_cell, &
                              self%ncells, &
                              self%cell_on_vert_2d, &
                              self%max_cells_per_vertex, &
                              self%nverts)

    ! Populate cells either side of each edge
    ! There can only ever be 2 cells incident on an edge (whatever the
    ! topography!)
    allocate( self%cell_on_edge_2d(2,self%nedges) )
    call calc_cell_on_edge( self%edge_on_cell_2d, &
                            self%nedges_per_cell, &
                            self%ncells, &
                            self%cell_on_edge_2d, &
                            self%nedges )

    ! Allocate each vertex to the cell with the highest global cell index
    ! of the cells neighbouring the vertex
    allocate( self%vert_cell_owner(self%nverts) )
    do ientity=1,self%nverts
      self%vert_cell_owner(ientity)=maxval( self%cell_on_vert_2d(:,ientity) )
    end do

    ! Allocate each edge to the cell with the highest global cell index
    ! of the cells neighbouring the edge
    allocate( self%edge_cell_owner(self%nedges) )
    do ientity=1,self%nedges
      self%edge_cell_owner(ientity)=maxval( self%cell_on_edge_2d(:,ientity) )
    end do

    ! Initialise values in this objects global mesh maps collection
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

    integer(i_def) :: nverts = 16
    integer(i_def) :: nedges = 24

    global_mesh_id_counter = global_mesh_id_counter + 1

    call self%set_id(global_mesh_id_counter)

    ! The feigner in the mesh unit test sets geometry to spherical,
    ! so make the global mesh used in the mesh unit tests consistent
    self%geometry  = spherical_domain
    self%topology  = non_periodic_domain
    self%coord_sys = lon_lat_coords
    self%mesh_name = 'planar:unit-test'

    ! Returns global_mesh_object of size 3x3 quad reference cell.
    ! As per reference cell, direction of numbering is anti-clockwise
    ! Starting point is
    ! Vertices: Bottom Left (south-west)
    ! Edges:    Left        (west)
    ! Faces:    Left        (west)
    self%ncells = 9

    self%nverts_per_cell = 4
    self%nedges_per_cell = 4

    self%max_cells_per_vertex  = 4

    allocate( self%cell_next_2d    (self%nedges_per_cell, self%ncells) )
    allocate( self%vert_on_cell_2d (self%nverts_per_cell, self%ncells) )
    allocate( self%edge_on_cell_2d (self%nedges_per_cell, self%ncells) )
    allocate( self%cell_on_vert_2d (self%max_cells_per_vertex, nverts) )
    allocate( self%cell_on_edge_2d (2, nedges) )
    allocate( self%vert_coords     (2, nverts) )
    allocate( self%cell_coords     (2, self%ncells) )
    allocate( self%vert_cell_owner (nverts) )
    allocate( self%edge_cell_owner (nedges) )

    self%ntarget_meshes = 0

    ! Note: These test coordinates are in [Long, Lat] in units of Radians
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

    self%cell_next_2d(:,1)     = [0, 0, 2, 4]
    self%cell_next_2d(:,2)     = [1, 0, 3, 5]
    self%cell_next_2d(:,3)     = [2, 0, 0, 6]
    self%cell_next_2d(:,4)     = [0, 1, 5, 7]
    self%cell_next_2d(:,5)     = [4, 2, 6, 8]
    self%cell_next_2d(:,6)     = [5, 3, 0, 9]
    self%cell_next_2d(:,7)     = [0, 4, 8, 0]
    self%cell_next_2d(:,8)     = [7, 5, 9, 0]
    self%cell_next_2d(:,9)     = [8, 6, 0, 0]

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

    self%cell_on_vert_2d(:,1)  = [0, 0, 1, 0]
    self%cell_on_vert_2d(:,2)  = [0, 0, 2, 1]
    self%cell_on_vert_2d(:,3)  = [0, 0, 3, 2]
    self%cell_on_vert_2d(:,4)  = [0, 0, 0, 3]
    self%cell_on_vert_2d(:,5)  = [0, 1, 4, 0]
    self%cell_on_vert_2d(:,6)  = [1, 2, 5, 4]
    self%cell_on_vert_2d(:,7)  = [2, 3, 6, 5]
    self%cell_on_vert_2d(:,8)  = [3, 0, 0, 6]
    self%cell_on_vert_2d(:,9)  = [0, 4, 7, 0]
    self%cell_on_vert_2d(:,10) = [4, 5, 8, 7]
    self%cell_on_vert_2d(:,11) = [5, 6, 9, 8]
    self%cell_on_vert_2d(:,12) = [6, 0, 0, 9]
    self%cell_on_vert_2d(:,13) = [0, 7, 0, 0]
    self%cell_on_vert_2d(:,14) = [7, 8, 0, 0]
    self%cell_on_vert_2d(:,15) = [8, 9, 0, 0]
    self%cell_on_vert_2d(:,16) = [9, 0, 0, 0]

    self%cell_on_edge_2d(:,1)  = [0, 1]
    self%cell_on_edge_2d(:,2)  = [0, 2]
    self%cell_on_edge_2d(:,3)  = [0, 3]
    self%cell_on_edge_2d(:,4)  = [1, 0]
    self%cell_on_edge_2d(:,5)  = [2, 1]
    self%cell_on_edge_2d(:,6)  = [3, 2]
    self%cell_on_edge_2d(:,7)  = [0, 3]
    self%cell_on_edge_2d(:,8)  = [1, 4]
    self%cell_on_edge_2d(:,9)  = [2, 5]
    self%cell_on_edge_2d(:,10) = [3, 6]
    self%cell_on_edge_2d(:,11) = [4, 0]
    self%cell_on_edge_2d(:,12) = [5, 4]
    self%cell_on_edge_2d(:,13) = [6, 5]
    self%cell_on_edge_2d(:,14) = [0, 6]
    self%cell_on_edge_2d(:,15) = [4, 7]
    self%cell_on_edge_2d(:,16) = [5, 8]
    self%cell_on_edge_2d(:,17) = [6, 9]
    self%cell_on_edge_2d(:,18) = [7, 0]
    self%cell_on_edge_2d(:,19) = [8, 7]
    self%cell_on_edge_2d(:,20) = [9, 8]
    self%cell_on_edge_2d(:,21) = [0, 9]
    self%cell_on_edge_2d(:,22) = [7, 0]
    self%cell_on_edge_2d(:,23) = [8, 0]
    self%cell_on_edge_2d(:,24) = [9, 0]

    self%vert_cell_owner(:) = [1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 7, 8, 9, 9]
    self%edge_cell_owner(:) = [1, 2, 3, 1, 2, 3, 3, 4, 5, 6, 4, 5, 6, 6,         &
                              7, 8, 9, 7, 8, 9, 9, 7, 8, 9]

    ! Initialise values in this objects global mesh maps collection
    if (.not. allocated(self%global_mesh_maps) )                 &
        allocate ( self%global_mesh_maps,                       &
                    source = global_mesh_map_collection_type() )

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
  ! Input:   vert_on_cell    Array with indices of vertices on cells
  !          verts_per_cell  Number of vertices per cell
  !          ncell           Number of cells
  !          cells_per_vert  Number of cells per vertex
  !          nvert           Number of vertices
  ! Output:  cell_on_vert    Array with indices of cells on vertices
  !-------------------------------------------------------------------------------
  subroutine calc_cell_on_vertex(vert_on_cell,  &
                                verts_per_cell, &
                                ncell,          &
                                cell_on_vert,   &
                                cells_per_vert, &
                                nvert)

  implicit none

  integer(i_def), intent(in)  :: verts_per_cell, ncell
  integer(i_def), intent(in)  :: vert_on_cell(verts_per_cell, ncell)
  integer(i_def), intent(in)  :: cells_per_vert, nvert
  integer(i_def), intent(out) :: cell_on_vert(cells_per_vert, nvert)

  integer(i_def) :: cell
  integer(i_def) :: vertno
  integer(i_def) :: cellno
  integer(i_def) :: vert

  cell_on_vert = 0

  ! There is no order to how the cell ids are listed around the vertex
  ! Some may have 4 or 3 (i.e. vertices on corners of panels)
  do cell=1, ncell
    do vertno=1, verts_per_cell

      vert = vert_on_cell(vertno,cell)

      do cellno=1, cells_per_vert
        if (cell_on_vert(cellno,vert) == cell) exit
        if (cell_on_vert(cellno,vert) == 0) then
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
  !          based on edge number with the cells around it
  ! Input:   edge_on_cell    Array with indices of edges on cells
  !          edges_per_cell  Number of edges per cell
  !          ncell           Number of cells
  !          nedge           Number of edges
  ! Output:  cell_on_edge    Array with indices of cells on edges
  !-------------------------------------------------------------------------------
  subroutine calc_cell_on_edge( edge_on_cell, &
                                edges_per_cell, &
                                ncell, &
                                cell_on_edge, &
                                nedge)
  implicit none

  integer(i_def), intent(in)  :: edges_per_cell, ncell
  integer(i_def), intent(in)  :: edge_on_cell(edges_per_cell, ncell)
  integer(i_def), intent(in)  :: nedge
  integer(i_def), intent(out) :: cell_on_edge(2, nedge)

  integer(i_def) :: cell
  integer(i_def) :: edgeno
  integer(i_def) :: cellno
  integer(i_def) :: edge

  cell_on_edge=0

  do cell=1,ncell
    do edgeno=1,edges_per_cell

      edge=edge_on_cell(edgeno,cell)

      do cellno=1, 2
        if(cell_on_edge(cellno,edge) == cell)exit
        if(cell_on_edge(cellno,edge) == 0)then
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
  !> @brief  Queries if the global mesh domain geometry is spherical
  !> @return answer .true. for a spherical domain surface
  !>
  function is_geometry_spherical( self ) result ( answer )

    implicit none

    class(global_mesh_type), intent(in) :: self

    logical (l_def) :: answer

    answer = ( self%geometry == spherical_domain )

  end function is_geometry_spherical


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Queries if the global mesh domain geometry is a flat surface.
  !> @return answer .true. for a flat domain surface
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
  !> @return answer .true. for a domain which is non-periodic
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
  !> @return answer .true. for a channel domain
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
  !> @return answer .true. for a fully periodic domain
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
  !> @return answer .true. if nodes are specified in Cartesian co-ordinates
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
  !> @return answer .true. if nodes are specified in spherical co-ordinates
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
  !> @brief  Returns values for the X and Y periodicity.
  !>
  !> @details periodic_x and periodic_y as read from the mesh file are
  !>          returned. This subroutine is currently only used during the
  !>          partitioning of planar meshes for determining mesh extents.
  !>          The values are obtained from the header of the mesh files
  !>          and are set to "false" for cubedsphere meshes (due to mesh
  !>          IO function requirements) and to a user defined value for
  !>          planar meshes.
  !>
  !> @param[out] periodic_x Mesh periodicity in E-W direction
  !> @param[out] periodic_y Mesh periodicity in N-S direction
  !>
  subroutine get_mesh_periodicity( self, periodic_x, periodic_y )
    implicit none

    class(global_mesh_type), intent(in) :: self
    logical(l_def), intent(out) :: periodic_x
    logical(l_def), intent(out) :: periodic_y

    periodic_x = self%periodic_x
    periodic_y = self%periodic_y

  end subroutine get_mesh_periodicity

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief  Returns ID of a cell in this mesh.
  !>
  !> @details Gets the cell ID that is x_cells across (E is +ve) and y_cells
  !>          up/down (N is +ve) from given cell_number.
  !>
  !> @note For a cubed -sphere mesh, this will only return correct cell IDs if
  !>       the offset cell remains on same the cubed-sphere "face" as the
  !>       start cell
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
  !> @brief Retruns the number of intergrid maps that exist for this mesh
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
  !> @return Total number of edges in the global domain.
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
  !> @return Total number of cells in the global domain.
  !>
  function get_ncells( self ) result (ncells)

  implicit none

  class(global_mesh_type), intent(in) :: self

  integer(i_def) :: ncells

  ncells = self%ncells

  end function get_ncells

  !---------------------------------------------------------------------------
  !> @brief Gets the maximum number of cells around a vertex.
  !>
  !> The actual number of cells at a particular vertex could be fewer than
  !> the value returned by this method.
  !>
  !> For example, in a cubed sphere mesh, there are generally four cells
  !> incident with a vertex except for the "corner" vertices where there are
  !> three.
  !>
  !> @return Maximum number of cells that can be incident with a vertex.
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
  !> @param[in] cell_gid Global ID of the cell being queried.
  !> @param[out] Edges around the given cell.
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
  !> @return edges [Edge,Cell] connectivity array in global ids
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
  !> @param[in] cell_gid Global ID of the cell being queried.
  !> @param[out] Vertices around the given cell.
  !>
  subroutine get_vert_on_cell(self, cell_gid, verts)

    implicit none
    class (global_mesh_type), intent(in)  :: self
    integer (i_def),          intent(in)  :: cell_gid
    integer (i_def),          intent(out) :: verts(:)

    verts(:) = self%vert_on_cell_2d(:,cell_gid)

  end subroutine get_vert_on_cell

  !---------------------------------------------------------------------------
  !> @brief Returns entire array for vertices on 2d cells.
  !> @return verts [vert,cell] connectivity array in global ids
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
  !> @param[in] cell_gid Global ID of a cell.
  !> @param[out] Global IDs of cells adjacent to cell cell_gid.
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
  !> @return cell_next [cell_next,cell] connectivity array in global ids
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
  subroutine get_vert_coords (self, vert_gid, vert_coords)

    implicit none
    class (global_mesh_type), intent(in)  :: self
    integer(i_def),           intent(in)  :: vert_gid
    real(r_def),              intent(out) :: vert_coords(:)

    vert_coords(1:2) = self%vert_coords(1:2,vert_gid)

  end subroutine get_vert_coords

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

    ! Perform tests to see if this is a valid map to add
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
    ! from source->target
    global_mesh_map =>                                                     &
        self%global_mesh_maps%get_global_mesh_map( source_global_mesh_id,  &
                                                  target_global_mesh_id )

    return

  end function get_global_mesh_map

  !---------------------------------------------------------------------------
  !> @brief Forced clear of all this oject from memory.
  !>
  !> Explicitly deallocates any allocatable arrays in the object to avoid
  !> memory leaks.
  !>
  !> This routine should not need to be called manually except (possibly) in
  !> pfunit tests
  !>
  subroutine clear(self)

    implicit none

    class (global_mesh_type), intent(inout) :: self

    if (allocated(self%vert_coords))       deallocate( self%vert_coords )
    if (allocated(self%cell_next_2d))      deallocate( self%cell_next_2d )
    if (allocated(self%vert_on_cell_2d))   deallocate( self%vert_on_cell_2d )
    if (allocated(self%cell_on_vert_2d))   deallocate( self%cell_on_vert_2d )
    if (allocated(self%edge_on_cell_2d))   deallocate( self%edge_on_cell_2d )
    if (allocated(self%cell_on_edge_2d))   deallocate( self%cell_on_edge_2d )
    if (allocated(self%vert_cell_owner))   deallocate( self%vert_cell_owner )
    if (allocated(self%edge_cell_owner))   deallocate( self%edge_cell_owner )
    if (allocated(self%global_mesh_maps))  deallocate( self%global_mesh_maps )

    return
  end subroutine clear

end module global_mesh_mod
