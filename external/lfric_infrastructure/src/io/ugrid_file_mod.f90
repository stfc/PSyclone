










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
  use local_mesh_map_collection_mod,  only: local_mesh_map_collection_type

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
  !> @param[in]   mesh_name              Name of mesh to read.
  !> @param[out]  geometry               Domain geometry enumeration key.
  !> @param[out]  coord_sys              Co-ordinate sys enumeration key.
  !> @param[out]  north_pole             [Longitude, Latitude] of north pole
  !>                                     for domain orientation (degrees).
  !> @param[out]  null_island            [Longitude, Latitude] of null
  !>                                     island for domain orientation (degrees).
  !> @param[out]  equatorial_latitude    Latitude of equator of mesh (degrees).
  !> @param[out]  node_coordinates       Node coordinates.
  !> @param[out]  face_coordinates       Face coordinates.
  !> @param[out]  coord_units_x          Units of x coordinates.
  !> @param[out]  coord_units_y          Units of y coordinates.
  !> @param[out]  void_cell              Values to mark a cell as external
  !>                                     to domain.
  !> @param[out]  face_node_connectivity Nodes around each face.
  !> @param[out]  edge_node_connectivity Nodes defining each edge.
  !> @param[out]  face_edge_connectivity Edges bounding each face.
  !> @param[out]  face_face_connectivity Faces adjacent to each face.
  !> @param[out]  topology            Domain topology enumeration key.
  !> @param[out]  periodic_xy         Periodicity in x/y axes.
  !> @param[out]  domain_extents      Principal coordinates that
  !>                                  describe the domain shape.
  !> @param[out]  npanels             Number of panels in this mesh.
  !> @param[out]  rim_depth           Depth in cells of global mesh rim
  !>                                  (LBC meshes).
  !> @param[out]  constructor_inputs  Inputs to the ugrid_generator used to
  !>                                  create the mesh.
  !> @param[out]  partition_of        Name of mesh that (local mesh) is a
  !>                                  partition of.
  !> @param[out]  num_faces_global    Number of cells in the global mesh
  !>                                  given by `partition_of`.
  !> @param[out]  max_stencil_depth   The max stencil depth that this
  !>                                  mesh supports.
  !> @param[out]  inner_depth         Depth (in cells) of inner halos inward
  !>                                  from edge cell layer.
  !> @param[out]  num_inner           Number of cells at each inner halo depth.
  !> @param[out]  last_inner_cell     Local ID of the last cell at each inner
  !>                                  halo depth.
  !> @param[out]  halo_depth          Depth (in cells) of halos outward from
  !>                                  edge cell layer.
  !> @param[out]  num_halo            Number of cells at each halo depth.
  !> @param[out]  last_halo_cell      Local ID of the last cell at each halo depth.
  !> @param[out]  num_edge            Number of cells in the edge cell layer.
  !> @param[out]  last_edge_cell      Local ID of the last cell in edge cell layer.
  !> @param[out]  num_ghost           Number of cells in ghost cell layer.
  !> @param[out]  last_ghost_cell     Local ID of the last cell in ghost cell layer.
  !> @param[out]  node_cell_owner     Cell that "owns" a given node.
  !> @param[out]  edge_cell_owner     Cell that "owns" a given edge.
  !> @param[out]  cell_gid            Global IDs of local mesh cells.
  !>                                  (partition, halos and ghost cells).
  !> @param[out]  node_on_cell_gid    Local node on cell connectivities listed with
  !>                                  the node global IDs.
  !> @param[out]  edge_on_cell_gid    Local edge on cell connectivities listed with
  !>                                  the edge global IDs.
  !> @param[out]  num_targets         Number of mesh maps from mesh.
  !> @param[out]  target_mesh_names   Mesh(es) that this mesh has maps for.
  !-----------------------------------------------------------------------------
  subroutine read_mesh_interface( self,                                   &
                       ! Common mesh related variables
                       mesh_name, geometry, coord_sys,                    &
                       north_pole, null_island, equatorial_latitude,      &
                       node_coordinates, face_coordinates,                &
                       coord_units_x, coord_units_y,                      &
                       void_cell,                                         &
                       face_node_connectivity, face_edge_connectivity,    &
                       face_face_connectivity, edge_node_connectivity,    &

                       ! Variables referring to global mesh types
                       topology, periodic_xy, domain_extents,             &
                       npanels, rim_depth, constructor_inputs,            &

                       ! Partition variables
                       partition_of, num_faces_global, max_stencil_depth, &
                       inner_depth, num_inner, last_inner_cell,           &
                       halo_depth,  num_halo,  last_halo_cell,            &
                       num_edge,  last_edge_cell,                         &
                       num_ghost, last_ghost_cell,                        &
                       node_cell_owner, edge_cell_owner,                  &
                       cell_gid, node_on_cell_gid, edge_on_cell_gid,      &

                       ! Intergrid maps
                       num_targets, target_mesh_names )

    import :: ugrid_file_type, i_def, r_def, str_def, str_longlong, &
              l_def

    implicit none

    ! Arguments
    class(ugrid_file_type), intent(inout) :: self

    ! Common mesh related variables
    character(str_def), intent(in)  :: mesh_name
    character(str_def), intent(out) :: geometry
    character(str_def), intent(out) :: coord_sys
    real(r_def),        intent(out) :: north_pole(2)
    real(r_def),        intent(out) :: null_island(2)
    real(r_def),        intent(out) :: equatorial_latitude
    real(r_def),        intent(out) :: node_coordinates(:,:)
    real(r_def),        intent(out) :: face_coordinates(:,:)
    character(str_def), intent(out) :: coord_units_x
    character(str_def), intent(out) :: coord_units_y
    integer(i_def),     intent(out) :: void_cell
    integer(i_def),     intent(out) :: face_node_connectivity(:,:)
    integer(i_def),     intent(out) :: face_edge_connectivity(:,:)
    integer(i_def),     intent(out) :: face_face_connectivity(:,:)
    integer(i_def),     intent(out) :: edge_node_connectivity(:,:)

    ! Variables referring to global mesh types
    character(str_def), intent(out) :: topology
    logical(l_def),     intent(out) :: periodic_xy(2)
    real(r_def),        intent(out) :: domain_extents(2,4)
    integer(i_def),     intent(out) :: npanels
    integer(i_def),     intent(out) :: rim_depth

    character(str_longlong), intent(out) :: constructor_inputs

    ! Partition variables
    character(str_def), intent(out) :: partition_of
    integer(i_def),     intent(out) :: num_faces_global
    integer(i_def),     intent(out) :: max_stencil_depth

    integer(i_def), intent(out) :: inner_depth
    integer(i_def), intent(out), allocatable :: num_inner(:)
    integer(i_def), intent(out), allocatable :: last_inner_cell(:)

    integer(i_def), intent(out) :: halo_depth
    integer(i_def), intent(out), allocatable :: num_halo(:)
    integer(i_def), intent(out), allocatable :: last_halo_cell(:)

    integer(i_def), intent(out) :: num_edge,  last_edge_cell
    integer(i_def), intent(out) :: num_ghost, last_ghost_cell

    integer(i_def), intent(out), allocatable :: node_cell_owner(:)
    integer(i_def), intent(out), allocatable :: edge_cell_owner(:)
    integer(i_def), intent(out), allocatable :: cell_gid(:)
    integer(i_def), intent(out), allocatable :: node_on_cell_gid(:,:)
    integer(i_def), intent(out), allocatable :: edge_on_cell_gid(:,:)

    integer(i_def), intent(out) :: num_targets

    character(str_def), intent(out), allocatable :: target_mesh_names(:)

  end subroutine read_mesh_interface

  !-----------------------------------------------------------------------------
  !> @brief  Interface: Writes mesh data in arguments to file.
  !> @details Writes required data to NetCDF UGRID mesh file:
  !>          * dimension
  !>          * coordinates
  !>          * connectivity information
  !>          * partition information
  !>          * mesh metadata
  !>
  !> @param[in]  mesh_name                Name of this mesh instance.
  !> @param[in]  geometry                 Domain geometry enumeration key.
  !> @param[in]  coord_sys                Co-ordinate sys enumeration key.
  !> @param[in]  north_pole               [Longitude, Latitude] of north pole
  !>                                      used for domain orientation (degrees).
  !> @param[in]  null_island              [Longitude, Latitude] of null
  !>                                      island used for domain orientation (degrees).
  !> @param[in]  equatorial_latitude      Latitude of equator of mesh.
  !> @param[in]  num_nodes                Number of nodes.
  !> @param[in]  num_edges                Number of edges.
  !> @param[in]  num_faces                Number of faces.
  !> @param[in]  node_coordinates         Node coordinates.
  !> @param[in]  face_coordinates         Face coordinates.
  !> @param[in]  coord_units_x            Units for x-coord.
  !> @param[in]  coord_units_y            Units for y-coord.
  !> @param[in]  void_cell                Value use to mark a cell as
  !>                                      external to domain bounds.
  !> @param[in]  face_node_connectivity   Nodes around each face
  !> @param[in]  face_edge_connectivity   Edges bounding each face
  !> @param[in]  face_face_connectivity   Faces adjacent to each face.
  !> @param[in]  edge_node_connectivity   Nodes defining each edge.
  !> @param[in]  topology                 Domain topology enumeration key.
  !> @param[in]  periodic_xy              Domain periodicity in x/y-axes.
  !> @param[in]  domain_extents           Principal coordinates that
  !>                                      describe the domain shape.
  !> @param[in]  npanels                  Number of panels in global mesh.
  !> @param[in]  rim_depth                Depth in cells of global mesh rim (LBC meshes).
  !> @param[in]  constructor_inputs       Inputs used to generate global mesh.
  !> @param[in]  partition_of             The mesh name of global mesh that
  !>                                      local mesh is a partition of.
  !> @param[in]  num_faces_global         The number of faces in global mesh that
  !>                                      local mesh is a partition of.
  !> @param[in]  max_stencil_depth        The maximum stencil depth supported by local mesh.
  !> @param[in]  inner_depth              Depth (in cells) of inner halos inward from edge cell layer.
  !> @param[in]  num_inner                Number of cells at each inner halo depth.
  !> @param[in]  last_inner_cell          Local id of the last cell at each inner halo depth.
  !> @param[in]  halo_depth               Depth (in cells) of halos outward from edge cell layer.
  !> @param[in]  num_halo                 Number of cells at each halo depth.
  !> @param[in]  last_halo_cell           Local id of the last cell at each halo depth.
  !> @param[in]  num_edge                 Number of cells in the partition edge cell layer.
  !> @param[in]  last_edge_cell           Local id of the last cell in the edge cell layer.
  !> @param[in]  num_ghost                Number of cells in the ghost cell layer.
  !> @param[in]  last_ghost_cell          Local id of the last cell in the ghost cell layer.
  !> @param[in]  node_cell_owner          Cell that "owns" a given node.
  !> @param[in]  edge_cell_owner          Cell that "owns" a given edge.
  !> @param[in]  cell_gid                 Global ids of local mesh cells
  !>                                      (partition, halos and ghost cells).
  !> @param[in]  node_on_cell_gid         Local node on cell connectivities listed with
  !>                                      the node global ids.
  !> @param[in]  edge_on_cell_gid         Local node on cell connectivities listed with
  !>                                      the node global ids.
  !> @param[in]  num_targets              Number of mesh maps from mesh.
  !> @param[in]  target_mesh_names        Mesh(es) that this mesh has maps for.
  !> @param[in]  target_global_mesh_maps  Mesh maps from this global mesh to target mesh(es).
  !> @param[in]  target_local_mesh_maps   Mesh maps from this local mesh to target mesh(es).
  !-----------------------------------------------------------------------------
  subroutine write_mesh_interface( self,                                  &

                       ! Common mesh type variables.
                       mesh_name, geometry, coord_sys,                    &
                       north_pole, null_island, equatorial_latitude,      &
                       num_nodes, num_edges, num_faces,                   &
                       node_coordinates, face_coordinates,                &
                       coord_units_x, coord_units_y, void_cell,           &
                       face_node_connectivity, face_edge_connectivity,    &
                       face_face_connectivity, edge_node_connectivity,    &

                       ! Global mesh variables.
                       topology, periodic_xy, domain_extents,             &
                       npanels, rim_depth, constructor_inputs,            &

                       ! Partition variables.
                       partition_of, num_faces_global, max_stencil_depth, &
                       inner_depth, num_inner, last_inner_cell,           &
                       halo_depth,  num_halo,  last_halo_cell,            &
                       num_edge,  last_edge_cell,                         &
                       num_ghost, last_ghost_cell,                        &
                       node_cell_owner, edge_cell_owner,                  &
                       cell_gid, node_on_cell_gid, edge_on_cell_gid,      &

                       ! Intergrid maps.
                       num_targets, target_mesh_names,                    &
                       target_global_mesh_maps, target_local_mesh_maps )

    import :: i_def, r_def, str_def, str_longlong, l_def, ugrid_file_type, &
              global_mesh_map_collection_type, local_mesh_map_collection_type

    implicit none

    ! Arguments.
    class(ugrid_file_type), intent(inout) :: self

    ! Common mesh variables.
    character(str_def), intent(in) :: mesh_name
    character(str_def), intent(in) :: geometry
    character(str_def), intent(in) :: coord_sys
    real(r_def),        intent(in) :: north_pole(2)
    real(r_def),        intent(in) :: null_island(2)
    real(r_def),        intent(in) :: equatorial_latitude

    integer(i_def),     intent(in) :: num_nodes
    integer(i_def),     intent(in) :: num_edges
    integer(i_def),     intent(in) :: num_faces
    real(r_def),        intent(in) :: node_coordinates(:,:)
    real(r_def),        intent(in) :: face_coordinates(:,:)
    character(str_def), intent(in) :: coord_units_x
    character(str_def), intent(in) :: coord_units_y
    integer(i_def),     intent(in) :: void_cell
    integer(i_def),     intent(in) :: face_node_connectivity(:,:)
    integer(i_def),     intent(in) :: face_edge_connectivity(:,:)
    integer(i_def),     intent(in) :: face_face_connectivity(:,:)
    integer(i_def),     intent(in) :: edge_node_connectivity(:,:)

    ! Global mesh only variables
    character(str_def), intent(in) :: topology
    logical(l_def),     intent(in) :: periodic_xy(2)
    real(r_def),        intent(in) :: domain_extents(2,4)
    integer(i_def),     intent(in) :: npanels
    integer(i_def),     intent(in) :: rim_depth

    character(str_longlong), intent(in) :: constructor_inputs

    ! Partition variables
    character(str_def), intent(in) :: partition_of
    integer(i_def),     intent(in) :: num_faces_global
    integer(i_def),     intent(in) :: max_stencil_depth

    integer(i_def), intent(in) :: inner_depth, num_inner(:), last_inner_cell(:)
    integer(i_def), intent(in) :: halo_depth,  num_halo(:),  last_halo_cell(:)
    integer(i_def), intent(in) :: num_edge,  last_edge_cell
    integer(i_def), intent(in) :: num_ghost, last_ghost_cell
    integer(i_def), intent(in) :: node_cell_owner(:)
    integer(i_def), intent(in) :: edge_cell_owner(:)
    integer(i_def), intent(in) :: cell_gid(:)
    integer(i_def), intent(in) :: node_on_cell_gid(:,:)
    integer(i_def), intent(in) :: edge_on_cell_gid(:,:)

    ! Inter-grid maps
    integer(i_def),     intent(in) :: num_targets
    character(str_def), intent(in), allocatable :: target_mesh_names(:)
    type(global_mesh_map_collection_type), &
                        intent(in), pointer :: target_global_mesh_maps
    type(local_mesh_map_collection_type),  &
                        intent(in), pointer :: target_local_mesh_maps

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
