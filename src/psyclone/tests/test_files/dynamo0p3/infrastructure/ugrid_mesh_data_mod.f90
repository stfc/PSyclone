!-----------------------------------------------------------------------------
! (c) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used
!-----------------------------------------------------------------------------

!> @brief  Stores the information required to populate global mesh object

!> @details Stores the information that is required to populate a global mesh
!>          object. The data within the object can be read from a global
!>          mesh file

module ugrid_mesh_data_mod

  use constants_mod,                  only: r_def, i_def, l_def, str_def
  use log_mod,                        only: log_event, log_scratch_space, &
                                            LOG_LEVEL_ERROR, LOG_LEVEL_TRACE

  implicit none

  private

  type, public :: ugrid_mesh_data_type
    !> Name of ugrid mesh topology
    character(str_def) :: global_mesh_name

    !> Domain geometry of global mesh.
    character(str_def) :: geometry
    character(str_def) :: topology
    character(str_def) :: coord_sys

    integer(i_def) :: max_stencil_depth

    !> Total number of nodes (vertices) in the full domain
    integer(i_def) :: nnode
    !> total number of cells in full domain
    integer(i_def) :: nface
    !> Total number of edges in the full domain
    integer(i_def) :: nedge
    !> Number of nodes (vertices) on each cell
    integer(i_def) :: num_nodes_per_face
    !> Number of nodes (vertices) on each edge
    integer(i_def) :: num_nodes_per_edge
    !> Number of edges on each cell
    integer(i_def) :: num_edges_per_face
    !> Maximum number of cells around a node (vertex)
    integer(i_def) :: max_num_faces_per_node
    !> Periodic in E-W direction
    logical(l_def) :: periodic_x
    !> Periodic in N-S direction
    logical(l_def) :: periodic_y
    !> Number of other "target" global meshes that this mesh is mapped to
    integer(i_def) :: ntarget_meshes
    !> Names of other "target" global meshes that this mesh is mapped to
    character(str_def),  allocatable :: target_global_mesh_names(:)
    !> Horizontal coords of nodes (vertices) in full domain
    real(r_def), allocatable :: node_coords(:,:)
    !> Horizontal coords of face (cell) centres in full domain
    real(r_def), allocatable :: face_coords(:,:)
    !> Full domain face to face (cell to cell) connectivities
    integer(i_def), allocatable :: face_next_2d(:,:)
    !> Full domain nodes (vertices) on a cell
    integer(i_def), allocatable :: node_on_face_2d(:,:)
    !> Full domain edges on a cell
    integer(i_def), allocatable :: edge_on_face_2d(:,:)

  contains
    procedure, public :: read_from_file
    procedure, public :: set_by_ugrid_2d
    procedure, public :: get_data
    procedure, public :: clear
    final :: ugrid_mesh_data_destructor

  end type ugrid_mesh_data_type

contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Reads the information that is required to populate a global mesh
  !>        object out of a global mesh file
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

    allocate( ncdf_quad_type :: file_handler )
    call ugrid_2d%set_file_handler( file_handler )
    call ugrid_2d%set_from_file_read( trim(global_mesh_name), trim(filename) )
    call set_by_ugrid_2d( self, ugrid_2d )

    if (allocated(file_handler)) deallocate( file_handler )

  end subroutine read_from_file


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Provides access to the data within the ugrid_mesh_data object - that
  !>        can be used to construct a global mesh object
  !>
  !> @param[out] mesh_name Name of ugrid mesh topology
  !> @param[out] geometry   Domain surface geometry
  !> @param[out] topology   Domain topology
  !> @param[out] coord_sys  Coordinate system used to position nodes
  !> @param[out] nnode Total number of nodes in the full domain
  !> @param[out] nedge Total number of edges in the full domain
  !> @param[out] nface total number of faces in full domain
  !> @param[out] nnodes_per_face Number of nodes on each face
  !> @param[out] nnodes_per_edge Number of nodes on each edge
  !> @param[out] nedges_per_face Number of edges on each cell
  !> @param[out] max_faces_per_node Maximum number of faces around a node
  !> @param[out] periodic_x Periodic in E-W direction
  !> @param[out] periodic_y Periodic in N-S direction
  !> @param[out] ntarget_meshes Number of target global mesh name
  !> @param[out] target_global_mesh_names Target of global mesh name
  !> @param[out] node_coords Horizontal coords of vertices in full domain
  !> @param[out] face_coords Horizontal coords of cell centres in full domain
  !> @param[out] face_next_2d Full domain cell to cell connectivities
  !> @param[out] node_on_face_2d Full domain vertices on a cell
  !> @param[out] edge_on_face_2d Full domain edges on a cell
  !> @param[out] max_stencil_depth Max stencil depth supported by this mesh
  !>
  subroutine get_data( self,      &
                       mesh_name, &
                       geometry,  &
                       topology,  &
                       coord_sys, &
                       nnode,     &
                       nedge,     &
                       nface,     &
                       nnodes_per_face, &
                       nnodes_per_edge, &
                       nedges_per_face, &
                       max_faces_per_node, &
                       periodic_x, &
                       periodic_y, &
                       ntarget_meshes, &
                       target_global_mesh_names, &
                       node_coords, &
                       face_coords, &
                       face_next_2d, &
                       node_on_face_2d, &
                       edge_on_face_2d, &
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

    logical(l_def), intent(out) :: periodic_x
    logical(l_def), intent(out) :: periodic_y
    integer(i_def), intent(out) :: ntarget_meshes
    character(str_def), intent(out),  allocatable :: target_global_mesh_names(:)
    real(r_def), intent(out), allocatable :: node_coords(:,:)
    real(r_def), intent(out), allocatable :: face_coords(:,:)
    integer(i_def), intent(out), allocatable :: face_next_2d(:,:)
    integer(i_def), intent(out), allocatable :: node_on_face_2d(:,:)
    integer(i_def), intent(out), allocatable :: edge_on_face_2d(:,:)

    ! Only valid if the ugrid file contains a local mesh
    integer(i_def), optional, intent(out) :: max_stencil_depth

    mesh_name = self%global_mesh_name
    geometry  = self%geometry
    topology  = self%topology
    coord_sys = self%coord_sys

    nnode = self%nnode
    nedge = self%nedge
    nface = self%nface
    nnodes_per_face = self%num_nodes_per_face
    nnodes_per_edge = self%num_nodes_per_edge
    nedges_per_face = self%num_edges_per_face
    max_faces_per_node = self%max_num_faces_per_node

    if (present(max_stencil_depth)) max_stencil_depth = self%max_stencil_depth

    periodic_x = self%periodic_x
    periodic_y = self%periodic_y
    ntarget_meshes = self%ntarget_meshes

    if ( ntarget_meshes > 0 ) then
      target_global_mesh_names = self%target_global_mesh_names
    end if
    node_coords = self%node_coords
    face_coords = self%face_coords
    face_next_2d = self%face_next_2d
    node_on_face_2d = self%node_on_face_2d
    edge_on_face_2d = self%edge_on_face_2d

  end subroutine get_data

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Destroys a ugrid_mesh_data object when it is finished with.
  !>
  subroutine clear(self)

    implicit none

    class (ugrid_mesh_data_type), intent(inout) :: self

    if (allocated(self%node_coords))       deallocate( self%node_coords )
    if (allocated(self%face_coords))       deallocate( self%face_coords )
    if (allocated(self%face_next_2d))      deallocate( self%face_next_2d )
    if (allocated(self%node_on_face_2d))   deallocate( self%node_on_face_2d )
    if (allocated(self%edge_on_face_2d))   deallocate( self%edge_on_face_2d )

  end subroutine clear

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Destructor clears the allocated data in the object
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

    call ugrid_2d%get_metadata                             &
            ( mesh_name         = self%global_mesh_name,   &
              geometry          = self%geometry,           &
              topology          = self%topology,           &
              coord_sys         = self%coord_sys,          &
              periodic_x        = self%periodic_x,         &
              periodic_y        = self%periodic_y,         &
              max_stencil_depth = self%max_stencil_depth,  &
              nmaps             = self%ntarget_meshes,     &
              target_mesh_names = self%target_global_mesh_names )

    allocate( self%node_coords(2, self%nnode) )
    call ugrid_2d%get_node_coords(self%node_coords)

    allocate( self%face_coords(2, self%nface) )
    self%face_coords = ugrid_2d%get_face_coords()

    allocate( self%face_next_2d( self%num_edges_per_face, self%nface ) )
    call ugrid_2d%get_face_face_connectivity( self%face_next_2d )

    allocate( self%node_on_face_2d( self%num_nodes_per_face, self%nface ) )
    call ugrid_2d%get_face_node_connectivity( self%node_on_face_2d )

    allocate( self%edge_on_face_2d( self%num_edges_per_face, self%nface ) )
    call ugrid_2d%get_face_edge_connectivity( self%edge_on_face_2d )

    return
  end subroutine set_by_ugrid_2d

end module ugrid_mesh_data_mod
