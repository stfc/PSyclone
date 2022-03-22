!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!>  @brief Abstract ugrid file type.
!>
!>  @details Provides an abstract ugrid file type, together with abstract
!>           procedure interfaces. Used to implement the OO strategy pattern.
!-------------------------------------------------------------------------------
module ugrid_file_mod

  use constants_mod, only: i_def, r_def, str_def, str_longlong, l_def
  use file_mod,      only: file_type
  use global_mesh_map_collection_mod, only: global_mesh_map_collection_type

  implicit none

  private

!-------------------------------------------------------------------------------
!> @brief Abstract ugrid file type
!>
!> @details  Defines the interface for a whole family of ugrid
!>           strategies, which extend this abstract type.
!-------------------------------------------------------------------------------

type, abstract, extends(file_type), public :: ugrid_file_type
  private
contains
  procedure (read_mesh_interface ),      deferred :: read_mesh
  procedure (read_map_interface ),       deferred :: read_map
  procedure (write_mesh_interface),      deferred :: write_mesh
  procedure (write_mesh_interface),      deferred :: append_mesh
  procedure (get_dimensions_interface),  deferred :: get_dimensions
  procedure (get_mesh_names_interface),  deferred :: get_mesh_names
  procedure (get_n_meshes_interface),    deferred :: get_n_meshes
  procedure (is_mesh_present_interface), deferred :: is_mesh_present

end type ugrid_file_type

!-------------------------------------------------------------------------------
! Abstract interfaces
!-------------------------------------------------------------------------------
abstract interface

  !-----------------------------------------------------------------------------
  !> @brief  Interface: Gets the intergrid map from two specified
  !>         mesh topologies in the file.
  !>
  !> @param[in]   self                   The ugrid file strategy object.
  !> @param[in]   source_mesh_name       Source mesh of the map
  !> @param[in]   target_mesh_name       Target mesh of the map
  !> @param[out]  mesh_map               Allocatable array of global mesh
  !>                                     ids. These map source mesh ids to
  !>                                     target mesh ids:
  !>                                     [n target cells per source cell,
  !>                                      n source cells]
  !-----------------------------------------------------------------------------
  subroutine read_map_interface( self,             &
                                 source_mesh_name, &
                                 target_mesh_name, &
                                 mesh_map )

    import :: ugrid_file_type, i_def, str_def

    implicit none

    ! Arguments
    class(ugrid_file_type), intent(in)  :: self
    character(str_def),     intent(in)  :: source_mesh_name
    character(str_def),     intent(in)  :: target_mesh_name

    integer(i_def),         intent(out), allocatable :: mesh_map(:,:,:)

  end subroutine read_map_interface


  !-----------------------------------------------------------------------------
  !> @brief  Interface: Gets numbers of nodes etc. for array dimensions.
  !>
  !> @param[in,out]  self                   The ugrid file strategy object.
  !> @param[in]      mesh_name              Mesh to get dimensions from
  !> @param[out]     num_nodes              Number of nodes
  !> @param[out]     num_edges              Number of edges
  !> @param[out]     num_faces              Number of faces
  !> @param[out]     num_nodes_per_face     Number of nodes per face
  !> @param[out]     num_edges_per_face     Number of edges per face
  !> @param[out]     num_nodes_per_edge     Number of nodes per edge
  !> @param[out]     max_num_faces_per_node Maximum number of faces per node
  !-----------------------------------------------------------------------------
  subroutine get_dimensions_interface( self,               &
                                       mesh_name,          &
                                       num_nodes,          &
                                       num_edges,          &
                                       num_faces,          &
                                       num_nodes_per_face, &
                                       num_edges_per_face, &
                                       num_nodes_per_edge, &
                                       max_num_faces_per_node )

    import :: ugrid_file_type, i_def, str_def

    implicit none

    ! Arguments
    class(ugrid_file_type), intent(inout) :: self

    character(str_def), intent(in)  :: mesh_name
    integer(i_def),     intent(out) :: num_nodes
    integer(i_def),     intent(out) :: num_edges
    integer(i_def),     intent(out) :: num_faces
    integer(i_def),     intent(out) :: num_nodes_per_face
    integer(i_def),     intent(out) :: num_edges_per_face
    integer(i_def),     intent(out) :: num_nodes_per_edge
    integer(i_def),     intent(out) :: max_num_faces_per_node

  end subroutine get_dimensions_interface

  !-----------------------------------------------------------------------------
  !> @brief  Interface: Queries the file for the names of meshes it content.
  !>
  !> @param[in]  self        The ugrid file object.
  !> @param[out] mesh_names  Character[:], Names of mesh topologies in file
  !-----------------------------------------------------------------------------
  subroutine get_mesh_names_interface( self, mesh_names )

    import :: ugrid_file_type, i_def

    implicit none

    ! Arguments
    class(ugrid_file_type),   intent(in)  :: self
    character(len=*),         intent(out) :: mesh_names(:)

  end subroutine get_mesh_names_interface

  !-----------------------------------------------------------------------------
  !> @brief  Interface: Queries the file for the names of meshes it content.
  !>
  !> @param[in]  self        The ugrid file object.
  !> @return     n_meshes    Integer, Number of mesh topologies in file
  !-----------------------------------------------------------------------------
  function get_n_meshes_interface( self ) result( n_meshes )

    import :: ugrid_file_type, i_def

    implicit none

    ! Arguments
    class(ugrid_file_type), intent(in)  :: self
    integer(i_def) :: n_meshes

  end function get_n_meshes_interface

  !-----------------------------------------------------------------------------
  !> @brief  Interface: Populates arguments with data from the mesh given
  !>         by `mesh_name` in the file to be read.
  !>
  !> @param[in,out] self                   The ugrid file strategy object.
  !> @param[in]     mesh_name              Name of mesh to read
  !> @param[out]    geometry               Domain geometry enumeration key
  !> @param[out]    topology               Domain topology enumeration key
  !> @param[out]    coord_sys              Co-ordinate sys enumeration key
  !> @param[out]    periodic_x             Periodic in E-W direction.
  !> @param[out]    periodic_y             Periodic in N-S direction.
  !> @param[out]    constructor_inputs     Inputs to the ugrid_generator used to
  !>                                       create the mesh
  !> @param[out]    node_coordinates       Node coordinates
  !> @param[out]    face_coordinates       Face coordinates
  !> @param[out]    coord_units_x          Units for x-coord
  !> @param[out]    coord_units_y          Units for y-coord
  !> @param[out]    face_node_connectivity Nodes around each face
  !> @param[out]    edge_node_connectivity Nodes defining each edge
  !> @param[out]    face_edge_connectivity Edges bounding each face
  !> @param[out]    face_face_connectivity Faces adjacent to each face.
  !> @param[out]    num_targets            Number of mesh maps from mesh
  !> @param[out]    target_mesh_names      Mesh(es) that this mesh has maps for
  !> @param[out]    north_pole             [Longitude, Latitude] of north pole
  !>                                       used for domain orientation (degrees)
  !> @param[out]    null_island            [Longitude, Latitude] of null
  !>                                       island used for domain orientation (degrees)
  !-----------------------------------------------------------------------------

  subroutine read_mesh_interface( self, mesh_name,                &
                                  geometry, topology, coord_sys,  &
                                  periodic_x, periodic_y,         &
                                  max_stencil_depth,              &
                                  constructor_inputs,             &
                                  node_coordinates,               &
                                  face_coordinates,               &
                                  coord_units_x, coord_units_y,   &
                                  face_node_connectivity,         &
                                  edge_node_connectivity,         &
                                  face_edge_connectivity,         &
                                  face_face_connectivity,         &
                                  num_targets, target_mesh_names, &
                                  north_pole, null_island  )

    import :: ugrid_file_type, i_def, r_def, str_def, str_longlong, &
              l_def

    implicit none

    ! Arguments
    class(ugrid_file_type), intent(inout) :: self

    character(str_def), intent(in)  :: mesh_name

    character(str_def), intent(out) :: geometry
    character(str_def), intent(out) :: topology
    character(str_def), intent(out) :: coord_sys
    logical(l_def),     intent(out) :: periodic_x
    logical(l_def),     intent(out) :: periodic_y
    integer(i_def),     intent(out) :: max_stencil_depth

    character(str_longlong), intent(out) :: constructor_inputs

    real(r_def),        intent(out) :: node_coordinates(:,:)
    real(r_def),        intent(out) :: face_coordinates(:,:)
    character(str_def), intent(out) :: coord_units_x
    character(str_def), intent(out) :: coord_units_y
    integer(i_def),     intent(out) :: face_node_connectivity(:,:)
    integer(i_def),     intent(out) :: edge_node_connectivity(:,:)
    integer(i_def),     intent(out) :: face_edge_connectivity(:,:)
    integer(i_def),     intent(out) :: face_face_connectivity(:,:)
    integer(i_def),     intent(out) :: num_targets
    character(str_def), intent(out), allocatable :: target_mesh_names(:)
    real(r_def),        intent(out) :: north_pole(2)
    real(r_def),        intent(out) :: null_island(2)

  end subroutine read_mesh_interface

  !-----------------------------------------------------------------------------
  !> @brief  Interface: Writes mesh data in arguments to file.
  !>
  !> @param[inout]   self                    The ugrid file strategy object.
  !> @param[in]      mesh_name               Name of this mesh instance
  !> @param[in]      geometry                Domain geometry enumeration key
  !> @param[in]      topology                Domain topology enumeration key
  !> @param[in]      coord_sys               Co-ordinate sys enumeration key
  !> @param[in]      periodic_x              Periodic in E-W direction.
  !> @param[in]      periodic_y              Periodic in N-S direction.
  !> @param[in]      constructor_inputs      Inputs used to generate mesh
  !> @param[in]      num_nodes               Number of nodes
  !> @param[in]      num_edges               Number of edges
  !> @param[in]      num_faces               Number of faces
  !> @param[in]      node_coordinates        Node coordinates
  !> @param[in]      face_coordinates        Face coordinates
  !> @param[in]      coord_units_x           Units for x-coord
  !> @param[in]      coord_units_y           Units for y-coord
  !> @param[in]      face_node_connectivity  Nodes around each face
  !> @param[in]      edge_node_connectivity  Nodes defining each edge
  !> @param[in]      face_edge_connectivity  Edges bounding each face
  !> @param[in]      face_face_connectivity  Faces adjacent to each face.
  !> @param[in]      num_targets             Number of mesh maps from mesh
  !> @param[in]      target_mesh_names       Mesh(es) that this mesh has maps for
  !> @param[in]      target_mesh_maps        Mesh maps from this mesh to target mesh(es)
  !> @param[in]      north_pole              [Longitude, Latitude] of north pole
  !>                                         used for domain orientation (degrees)
  !> @param[in]      null_island             [Longitude, Latitude] of null
  !>                                         island used for domain orientation (degrees)
  !-----------------------------------------------------------------------------

  subroutine write_mesh_interface( self, mesh_name, geometry, topology, coord_sys, &
                                   periodic_x, periodic_y, max_stencil_depth,      &
                                   constructor_inputs,                             &
                                   num_nodes, num_edges, num_faces,                &
                                   node_coordinates, face_coordinates,             &
                                   coord_units_x, coord_units_y,                   &
                                   face_node_connectivity,                         &
                                   edge_node_connectivity,                         &
                                   face_edge_connectivity,                         &
                                   face_face_connectivity,                         &
                                   num_targets,                                    &
                                   target_mesh_names,                              &
                                   target_mesh_maps,                               &
                                   north_pole,                                     &
                                   null_island )

    import :: ugrid_file_type, i_def, r_def, str_def, str_longlong, l_def, &
              global_mesh_map_collection_type

    implicit none

    ! Arguments
    class(ugrid_file_type), intent(inout) :: self

    character(str_def), intent(in) :: mesh_name

    character(str_def), intent(in) :: geometry
    character(str_def), intent(in) :: topology
    character(str_def), intent(in) :: coord_sys
    logical(l_def),     intent(in) :: periodic_x
    logical(l_def),     intent(in) :: periodic_y

    integer(i_def),     intent(in) :: max_stencil_depth

    character(str_longlong), intent(in) :: constructor_inputs

    integer(i_def),     intent(in) :: num_nodes
    integer(i_def),     intent(in) :: num_edges
    integer(i_def),     intent(in) :: num_faces
    real(r_def),        intent(in) :: node_coordinates(:,:)
    real(r_def),        intent(in) :: face_coordinates(:,:)
    character(str_def), intent(in) :: coord_units_x
    character(str_def), intent(in) :: coord_units_y
    integer(i_def),     intent(in) :: face_node_connectivity(:,:)
    integer(i_def),     intent(in) :: edge_node_connectivity(:,:)
    integer(i_def),     intent(in) :: face_edge_connectivity(:,:)
    integer(i_def),     intent(in) :: face_face_connectivity(:,:)
    integer(i_def),     intent(in) :: num_targets
    character(str_def), intent(in), allocatable :: target_mesh_names(:)
    type(global_mesh_map_collection_type), &
                        intent(in) :: target_mesh_maps

    real(r_def),        intent(in) :: north_pole(2)
    real(r_def),        intent(in) :: null_island(2)

  end subroutine write_mesh_interface

  !-----------------------------------------------------------------------------
  !>  @brief    Interface: Function to determine if a given mesh is present
  !>            in NetCDF ugrid file
  !>
  !>  @param[in] self        The netCDF file object.
  !>  @param[in] mesh_name   Name of the mesh topology
  !>  @return    answer      .True. if mesh_name is present in file
  !-----------------------------------------------------------------------------
  function is_mesh_present_interface(self, mesh_name) result(answer)

    import :: ugrid_file_type, l_def, str_def

    implicit none

    class(ugrid_file_type), intent(in) :: self
    character(str_def),     intent(in) :: mesh_name
    logical(l_def) :: answer

  end function is_mesh_present_interface

end interface

end module ugrid_file_mod
