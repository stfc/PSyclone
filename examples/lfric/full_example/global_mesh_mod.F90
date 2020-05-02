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
                                            str_def, degrees_to_radians, l_def
  use linked_list_data_mod,           only: linked_list_data_type
  use log_mod,                        only: log_event, log_scratch_space, &
                                            LOG_LEVEL_ERROR, LOG_LEVEL_TRACE
  implicit none

  private

  type, extends(linked_list_data_type), public :: global_mesh_type
    private

  ! Type of mesh that the global mesh describes
    character(str_def) :: mesh_class
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

  contains
    procedure, public :: get_mesh_class
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
  !> @param[in] global_mesh_name Name of ugrid mesh topology to create
  !>                             global mesh object from.
  !>
  !> @return Freshly minted global_mesh_type object.
  !>
  function global_mesh_constructor( filename, global_mesh_name ) result(self)

    implicit none
    character(*), intent(in) :: filename
    character(*), intent(in) :: global_mesh_name
    type(global_mesh_type) :: self

    self = global_mesh_constructor_unit_test_data()
    
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
    self%mesh_class = 'sphere'

    ! Returns global_mesh_object of size 3x3 quad reference cell.
    ! As per reference cell, direction of numbering is anti-clockwise
    ! Starting point is
    ! Vertices: Bottom Left (south-west)
    ! Edges:    Left        (west)
    ! Faces:    Left        (west)
    self%ncells = 9
    self%nverts = nverts
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

    self%cell_next_2d(:,1)     = [-1, -1,  2,  4]
    self%cell_next_2d(:,2)     = [ 1, -1,  3,  5]
    self%cell_next_2d(:,3)     = [ 2, -1, -1,  6]
    self%cell_next_2d(:,4)     = [-1,  1,  5,  7]
    self%cell_next_2d(:,5)     = [ 4,  2,  6,  8]
    self%cell_next_2d(:,6)     = [ 5,  3, -1,  9]
    self%cell_next_2d(:,7)     = [-1,  4,  8, -1]
    self%cell_next_2d(:,8)     = [ 7,  5,  9, -1]
    self%cell_next_2d(:,9)     = [ 8,  6, -1, -1]

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
  !> @brief  Returns string identifying the class of mesh.
  !>
  !> @details Currently only 'sphere', 'planar' mesh classes recognised.
  !>
  !> @return mesh_class Type of mesh_class this global_mesh describes.
  !>
  function get_mesh_class( self ) result ( mesh_class )

    implicit none

    class(global_mesh_type), intent(in) :: self

    character(str_def) :: mesh_class

    mesh_class = self%mesh_class

  end function get_mesh_class

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
  !> @param[in] edge_number Number of the edge being queried.
  !> @param[out] cells Cells either side of the given edge.
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

    return
  end subroutine clear

end module global_mesh_mod
