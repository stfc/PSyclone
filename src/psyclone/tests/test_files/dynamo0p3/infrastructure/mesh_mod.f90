! Modifications copyright (c) 2017, Science and Technology Facilities Council
!------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!------------------------------------------------------------------------------
!
!> Module for mesh_type which defines local 3D mesh object.
!>
!> This module provides details for a mesh_type which is generated
!> using a global mesh object and a partition object, along some with
!> inputs that describe the vertical structure.
!>
!> It is also contains a static mesh object for unit testing. This
!> is returned if a mesh_object is instatiated with a single integer
!> argument.

module mesh_mod

  use constants_mod,        only : i_def, r_def, l_def, pi, imdi
  use linked_list_data_mod, only : linked_list_data_type
  use partition_mod, only: partition_type

  implicit none

  private

  type, extends(linked_list_data_type), public :: mesh_type

    private

    !> The partition object that describes this local
    !> mesh's partition of the global mesh
    type(partition_type) :: partition

    !> The domain limits (x,y,z) for Cartesian domains
    !>                   (long, lat, radius) for spherical
    !type (domain_size_type) :: domain_size

    !> Number of 3d-cell layers in mesh object
    integer(i_def) :: nlayers

    !> Top of atmosphere above surface
    real(r_def) :: domain_top

    !> Non-dimensional vertical coordinate eta[0,1], eta(0:nlayers)
    real(r_def), allocatable :: eta(:)

    !> Depth of 3d-cell layer [m], dz(nlayers)
    real(r_def), allocatable :: dz(:)

    !> Vertex Coordinates
    !> The x-, y- and z-coordinates of vertices on the mesh [m]
    real(r_def), allocatable :: vertex_coords(:,:)

    !==========================================================================
    ! Mesh properties:
    !==========================================================================
    ! Local partition base level
    integer(i_def) :: nverts_2d            !< Number of verts in partition
    integer(i_def) :: nedges_2d            !< Number of edges in partition
    integer(i_def) :: ncells_2d            !< Number of cells in partition
    integer(i_def) :: ncells_2d_with_ghost !< Number of cells in partition
                                           !< @b including ghost cells

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
    ! Local/Global id Maps
    !==========================================================================
    ! Map is only for the base level, i.e. the surface level.
    ! The index of the array is taken to be the entities local id.
    integer(i_def), allocatable :: cell_lid_gid_map(:)

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

    !==========================================================================
    ! Colouring storage: these form the arguments to set_colours().
    !==========================================================================
    !> integer, the number of colours
    integer(i_def),              private :: ncolours
    !> integer 1-d array, how many cells belong to each colour
    integer(i_def), allocatable, private :: ncells_per_colour(:)
    !> integer 2-d array, which cells are in each colour.
    integer(i_def), allocatable, private :: cells_in_colour(:,:)

  contains

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
    procedure, public :: get_cell_coords
    procedure, public :: get_column_coords
    procedure, public :: get_nverts_per_cell
    procedure, public :: get_nedges_per_cell
    procedure, public :: get_nfaces_per_cell
    procedure, public :: get_cell_gid
    procedure, public :: get_cell_lid
    procedure, public :: get_cell_next
    procedure, public :: get_face_on_cell
    procedure, public :: get_edge_on_cell
    procedure, public :: get_vert_on_cell
    !procedure, public :: get_domain_size
    procedure, public :: get_domain_top
    procedure, public :: get_dz
    procedure, public :: get_eta
    procedure, public :: get_vertex_cell_owner
    procedure, public :: get_edge_cell_owner
    procedure, public :: is_vertex_owned
    procedure, public :: is_edge_owned
    procedure, public :: is_cell_owned
    procedure, public :: get_inner_depth
    procedure, public :: get_num_cells_inner
    procedure, public :: get_last_inner_cell
    procedure, public :: get_num_cells_edge
    procedure, public :: get_last_edge_cell
    procedure, public :: get_halo_depth
    procedure, public :: get_num_cells_halo
    procedure, public :: get_last_halo_cell
    procedure, public :: get_num_cells_ghost
    procedure, public :: get_gid_from_lid

    ! get total_ranks and local_rank from partition

    procedure, public :: get_total_ranks
    procedure, public :: get_local_rank

    ! Colouring now accessed through function_space_type
    procedure, public :: set_colours
    procedure, public :: get_ncolours
    procedure, public :: get_colours
    procedure, public :: is_coloured

    ! Destructor frees colouring storage
    !final :: mesh_destructor

  end type mesh_type

  ! -------------------------------------------------------------------------
  ! Module parameters
  ! -------------------------------------------------------------------------
  
  !> Counter variable to keep track of the next mesh id number to uniquely 
  !! identify each different mesh
  integer(i_def) :: mesh_id_counter = 0

  !============================================================================
  ! Options for horizontal pFUnit test meshes
  !============================================================================
  !
  !> @}
  !> @name Horizontal Grid Types for pFunit tests
  integer(i_def), parameter, public :: PLANE             = 1
  integer(i_def), parameter, public :: PLANE_BI_PERIODIC = 2
  !> @}

contains

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


  !> @details This function returns the number of vertices in the local 3d-mesh.
  !> @return  Total number of vertices in the mesh object
  !============================================================================
  function get_nverts(self) result (nverts)

    ! Returns total number of vertices in this 3d-mesh

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


  !> @details This function returns the number of faces in the local 3d-mesh.
  !> @return  Total number of faces in the mesh object
  !============================================================================
  function get_nfaces(self) result (nfaces)

    ! Returns total number of faces in this 3d-mesh

    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nfaces

    nfaces = self%nfaces

  end function get_nfaces


  !> @details This function returns the global (across all processors) cell id
  !>          of a 2d-cell (i.e. on the local partition) with a local id given
  !>          by @c cell_lid .
  !> @param [in] cell_lid  The local id of the requested cell
  !> @return Global id of cell specified by @c cell_lid
  !============================================================================
  function get_cell_gid(self, cell_lid) result (cell_gid)

    ! Returns global cell id of a local 2d-cell on the partition
    ! that this mesh is based upon

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: cell_lid
    integer(i_def)               :: cell_gid

    cell_gid = self%cell_lid_gid_map(cell_lid)

  end function get_cell_gid


  !> @details This function returns the local (to partition) cell id
  !>          of a 2d-cell with the global (across all processors) cell
  !>          id given by @c cell_gid .
  !> @param [in] cell_gid  Global id of the requested 2d-cell
  !> @return The local id of cell with global id @c cell_gid
  !============================================================================
  function get_cell_lid(self, cell_gid) result (cell_lid)

    ! Returns local cell id on this mesh of a given global
    ! cell id. If the global cell does not exist on this mesh,
    ! IMDI is returned

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: cell_gid
    integer(i_def)               :: cell_lid

    integer(i_def) :: i

    cell_lid = IMDI
    do i=1, self%ncells_2d
      if (self%cell_lid_gid_map(i) == cell_gid) then
        cell_lid = i
        exit
      end if
    end do

  end function get_cell_lid

  !> @details Returns the height above surface of the top of the mesh domain
  !> @return  Height of top of mesh
  !============================================================================
  function get_domain_top(self) result (domain_top)

    ! Returns top of model height above surface

    implicit none
    class (mesh_type), intent(in) :: self
    real  (r_def)                 :: domain_top

    domain_top = self%domain_top

  end function get_domain_top


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
  !>                 vertical coordinate normalised using the @c domain_top
  !============================================================================
  subroutine get_eta(self,eta)

    ! Returns non-dimensional vertical coordinate eta[0,1]

    implicit none
    class (mesh_type), intent(in) :: self
    real(r_def),      intent(out) :: eta(:)

    ! Get the thickness of layers
    eta(:) = self%eta(:)

  end subroutine get_eta


  !> @return  Domain size of mesh as a <domain_type>
  !============================================================================
!!$  function get_domain_size(self) result (domain_size)
!!$
!!$    ! Returns local mesh domain limits as a domain_size_type type
!!$
!!$    implicit none
!!$    class(mesh_type), intent(in) :: self
!!$    type(domain_size_type)          :: domain_size
!!$
!!$    domain_size = self%domain_size
!!$
!!$  end function get_domain_size


  !> @param[in] vertex_index  The index of the vertex entity on the cell with
  !>                          local_id of cell_lid.
  !> @param[in] cell_lid      The local cell id on which the vertex entity is
  !>                          associated with.
  !> @return                  Gets the local id of the cell that "owns" a
  !>                          particular vertex entity
  !============================================================================
  function get_vertex_cell_owner( self, vertex_index, cell_lid ) &
                                  result (cell_owner)

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: vertex_index
    integer(i_def),   intent(in) :: cell_lid
    integer(i_def)               :: cell_owner

    cell_owner = self%vert_cell_owner( vertex_index, cell_lid )

  end function get_vertex_cell_owner


  !> @param[in] edge_index  The index of the edge entity on the cell with
  !>                        local_id of cell_lid.
  !> @param[in] cell_lid    The local cell id on which the edge entity is
  !>                        associated with.
  !> @return                Gets the local id of the cell that "owns" a
  !>                        particular edge entity
  !============================================================================
  function get_edge_cell_owner( self, edge_index, cell_lid ) &
                                result (cell_owner)

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: edge_index
    integer(i_def),   intent(in) :: cell_lid
    integer(i_def)               :: cell_owner

    cell_owner = self%edge_cell_owner( edge_index, cell_lid )

  end function get_edge_cell_owner


  !> @param[in] vertex_index  The index of the vertex entity on the cell with
  !>                          local_id of cell_lid.
  !> @param[in] cell_lid      The local cell id on which the edge entity is
  !>                          associated with.
  !> @return                  Whether the vertex is owned by the
  !>                          local parition
  !============================================================================
  function is_vertex_owned( self, vertex_index, cell_lid ) result (owned)

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: vertex_index
    integer(i_def),   intent(in) :: cell_lid
    logical(l_def)               :: owned

    owned = .false.
    if ( self%vertex_ownership( vertex_index, cell_lid ) == &
         self%partition%get_local_rank() ) owned = .true.

  end function is_vertex_owned


  !> @param[in] edge_index  The index of the edge entity on the cell with
  !>                        local_id of cell_lid.
  !> @param[in] cell_lid    The local cell id which the edge entity is
  !>                        associated with.
  !> @return                Whether the edge is owned by the local parition
  !============================================================================
  function is_edge_owned( self, edge_index, cell_lid ) result (owned)

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: edge_index
    integer(i_def),   intent(in) :: cell_lid
    logical(l_def)               :: owned

    owned = .false.
    if ( self%edge_ownership( edge_index, cell_lid ) == &
         self%partition%get_local_rank() ) owned = .true.

  end function is_edge_owned


  !> @param[in] cell_lid  The local cell id on the local partition.
  !> @return              Whether the cell is owned by the local parition
  !============================================================================
  function is_cell_owned( self, cell_lid ) result (owned)

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: cell_lid
    logical(l_def)               :: owned

    owned = .false.
    if ( self%partition%get_cell_owner(cell_lid) == &
         self%partition%get_local_rank())    owned = .true.

  end function is_cell_owned

  !> Returns the maximum depth of the inner halos from the partition object
  !> @return inner_halo_depth The maximum depth of the inner halo cells
  !============================================================================
  function get_inner_depth( self ) result ( inner_depth )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def) :: inner_depth

    inner_depth = self%partition%get_inner_depth()

  end function get_inner_depth

  !> Returns the total number of inner halo cells in a particular depth of
  !> inner halo in a 2d slice from the partition object
  !> @param[in] depth The depth of the inner halo being queried
  !> @return inner_halo_cells The total number of inner halo cells of the
  !>                          particular depth on the local partition
  !============================================================================
  function get_num_cells_inner( self, depth ) result ( inner_cells )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def)             :: inner_cells

    if( depth > self%get_inner_depth() )then
      inner_cells = 0
    else
      inner_cells = self%partition%get_num_cells_inner(depth)
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

    if( depth > self%get_inner_depth() )then
      last_inner_cell = 0
    else
      last_inner_cell = self%partition%get_last_inner_cell(depth)
    end if

  end function get_last_inner_cell

  !> Get the number of edge cells from the partition object
  !> @return edge_cells The total number of edge cells on the 
  !> local partition
  !============================================================================
  function get_num_cells_edge( self ) result ( edge_cells )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def) :: edge_cells

    edge_cells = self%partition%get_num_cells_edge()

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

    last_edge_cell = self%partition%get_last_edge_cell()

  end function get_last_edge_cell


  !> @details Returns the maximum depth of the halo from the partition object
  !> @return  The maximum depth of halo cells
  !============================================================================
  function get_halo_depth( self ) result ( halo_depth )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def)               :: halo_depth

    halo_depth = self%partition%get_halo_depth()

  end function get_halo_depth


  !> @details Returns the total number of halo cells in a particular depth
  !>          of halo in a 2d slice from the partition object
  !> @param[in] depth       The depth of the halo being queried
  !> @return                The total number of halo cells of the particular
  !>                        depth on the local partition
  !============================================================================
  function get_num_cells_halo( self, depth ) result ( halo_cells )

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: depth
    integer(i_def)               :: halo_cells

    if (depth > self%get_halo_depth()) then
      halo_cells = 0
    else
      halo_cells = self%partition%get_num_cells_halo(depth)
    end if

  end function get_num_cells_halo

  !> @brief  Gets the index of the last cell in a halo 
  !> @details Returns the index of the last cell in a particular depth
  !>          of halo in a 2d slice on the local partition
  !> @param[in] depth The depth of the halo being queried
  !> @return last_halo_cell The index of the last cell in the particular depth
  !>         of halo on the local partition
  !============================================================================
  function get_last_halo_cell( self, depth ) result ( last_halo_cell )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def)             :: last_halo_cell

    if( depth > self%get_halo_depth() )then
      last_halo_cell = 0
    else
      last_halo_cell = self%partition%get_last_halo_cell(depth)
    end if

  end function get_last_halo_cell

  !> @details Get the total number of ghost cells in a slice around
  !>          the local partition
  !> @return  The total number of ghost cells around the local partition
  !============================================================================
  function get_num_cells_ghost( self ) result ( ghost_cells )

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def)               :: ghost_cells

    ghost_cells = self%partition%get_num_cells_ghost()

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

    cell_gid = self%partition%get_gid_from_lid(cell_lid)

  end function get_gid_from_lid

  !> @details Gets the total ranks from the partition
  !> @return              The total ranks
  !============================================================================
  function get_total_ranks( self ) result ( total_ranks )

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def)               :: total_ranks 

    total_ranks = self%partition%get_total_ranks()

  end function get_total_ranks

  !> @details fets the local rank from the partition
  !> @return              The local rank
  !============================================================================
  function get_local_rank( self ) result ( local_rank )

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def)               :: local_rank

    local_rank = self%partition%get_local_rank()

  end function get_local_rank


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

  !============================================================================
  !> @brief  Invoke calculation of colouring for this mesh.
  !>
  !> @param[in] self  The mesh_type instance.
  !============================================================================
  subroutine set_colours(self)
    !use mesh_colouring_mod, only : colour_mod_set_colours => set_colours
    implicit none
    class(mesh_type), intent(inout) :: self


!!$    call colour_mod_set_colours(self%get_ncells_2d(), &
!!$                                self%cell_next, &
!!$                                self%ncolours, &
!!$                                self%ncells_per_colour, &
!!$                                self%cells_in_colour)

  end subroutine set_colours

end module mesh_mod
