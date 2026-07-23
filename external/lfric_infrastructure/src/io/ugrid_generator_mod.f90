










!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief   Abstract mesh generator type.
!> @details Provides an abstract mesh generator type, together with abstract
!>          procedure interfaces.  Used to implement the OO strategy pattern.
!-------------------------------------------------------------------------------

module ugrid_generator_mod

use constants_mod,                  only: r_def, i_def, str_def, l_def, &
                                          str_longlong
use global_mesh_map_collection_mod, only: global_mesh_map_collection_type

implicit none

private

!-------------------------------------------------------------------------------
!> @brief Abstract generator type
!-------------------------------------------------------------------------------
type, abstract, public :: ugrid_generator_type
  private

contains
  procedure ( generate_interface         ),     deferred :: generate
  procedure ( get_npanels_interface      ),     deferred :: get_number_of_panels
  procedure ( get_metadata_interface     ),     deferred :: get_metadata
  procedure ( get_dimensions_interface   ),     deferred :: get_dimensions
  procedure ( get_coordinates_interface  ),     deferred :: get_coordinates
  procedure ( get_connectivity_interface ),     deferred :: get_connectivity
  procedure ( get_global_mesh_maps_interface ), deferred :: get_global_mesh_maps
end type ugrid_generator_type

!-------------------------------------------------------------------------------
! Abstract interfaces
!-------------------------------------------------------------------------------

abstract interface

  !-----------------------------------------------------------------------------
  !> @brief Function interface for requesting global_mesh_maps collection
  !>        connected with the ugrid_generator
  !>
  !> @return global_mesh_maps  Pointer global_mesh_maps collection.
  !-----------------------------------------------------------------------------
  function get_global_mesh_maps_interface (self) result (global_mesh_maps)

    import :: ugrid_generator_type, global_mesh_map_collection_type

    implicit none

    class(ugrid_generator_type), target, intent(in) :: self
    type(global_mesh_map_collection_type), pointer :: global_mesh_maps

  end function get_global_mesh_maps_interface


  !-----------------------------------------------------------------------------
  !> @brief Function interface for number of panels in a resulting mesh
  !>        topology
  !>
  !> @return answer  Integer number of panels on mesh
  !-----------------------------------------------------------------------------
  function get_npanels_interface (self) result (answer)

    import :: ugrid_generator_type, i_def

    implicit none

    class(ugrid_generator_type), intent(in) :: self
    integer(i_def) :: answer

  end function get_npanels_interface


  !-----------------------------------------------------------------------------
  !> @brief Interface: runs the mesh generator strategy.
  !>
  !> @param[in,out] self  The generator strategy object.
  !-----------------------------------------------------------------------------
  subroutine generate_interface (self)

    import :: ugrid_generator_type

    implicit none

    class(ugrid_generator_type), intent(inout) :: self

  end subroutine generate_interface


  !-----------------------------------------------------------------------------
  !> @brief Interface: Returns mesh metadata information.
  !>
  !> @param[out]  mesh_name           Optional: Name of mesh instance to generate.
  !> @param[out]  geometry            Optional: Domain geometry enumeration key.
  !> @param[out]  topology            Optional: Domain topology enumeration key.
  !> @param[out]  coord_sys           Optional: Co-ordinate sys enumeration key.
  !> @param[out]  periodic_xy         Optional: Periodic in x/y axes.
  !> @param[out]  edge_cells_x        Optional: Number of panel edge cells (x-axis).
  !> @param[out]  edge_cells_y        Optional: Number of panel edge cells (y-axis).
  !> @param[out]  constructor_inputs  Optional: Inputs used to create this mesh from
  !>                                            the this ugrid_generator_type.
  !> @param[out]  nmaps               Optional: Number of maps to create with this mesh
  !>                                            as source mesh.
  !> @param[out]  rim_depth           Optional: Rim depth of LBC mesh (LAMs).
  !> @param[out]  void_cell           Optional: Cell ID to mark null cell connectivity.
  !> @param[out]  target_mesh_names   Optional: Mesh names of the target meshes that
  !>                                            this mesh has maps for.
  !> @param[out]  maps_edge_cells_x   Optional: Number of panel edge cells (x-axis) of
  !>                                            target mesh(es) to create map(s) for.
  !> @param[out]  maps_edge_cells_y   Optional: Number of panel edge cells (y-axis) of
  !>                                            target mesh(es) to create map(s) for.
  !> @param[out]  north_pole          Optional: [Longitude, Latitude] of north pole
  !>                                            used in for domain orientation (degrees).
  !> @param[out]  null_island         Optional: [Longitude, Latitude] of null
  !>                                            island used for domain orientation (degrees).
  !> @param[out]  equatorial_latitude Optional: latitude of equator of mesh (degrees)
  !-----------------------------------------------------------------------------
  subroutine get_metadata_interface ( self, mesh_name,                       &
                                      geometry, topology, coord_sys,         &
                                      periodic_xy,                           &
                                      edge_cells_x, edge_cells_y,            &
                                      constructor_inputs, nmaps, rim_depth,  &
                                      void_cell, target_mesh_names,          &
                                      maps_edge_cells_x, maps_edge_cells_y,  &
                                      north_pole, null_island, equatorial_latitude  )

    import :: ugrid_generator_type, i_def, str_def, str_longlong, l_def, r_def

    implicit none

    class(ugrid_generator_type),  intent(in)  :: self

    character(str_def), optional, intent(out) :: mesh_name
    character(str_def), optional, intent(out) :: geometry
    character(str_def), optional, intent(out) :: topology
    character(str_def), optional, intent(out) :: coord_sys
    logical(l_def),     optional, intent(out) :: periodic_xy(2)

    character(str_longlong), optional, intent(out) :: constructor_inputs

    character(str_def), allocatable, &
                    optional, intent(out) :: target_mesh_names(:)
    integer(i_def), allocatable, &
                    optional, intent(out) :: maps_edge_cells_x(:)
    integer(i_def), allocatable, &
                    optional, intent(out) :: maps_edge_cells_y(:)

    integer(i_def), optional, intent(out) :: nmaps
    integer(i_def), optional, intent(out) :: rim_depth
    integer(i_def), optional, intent(out) :: edge_cells_x
    integer(i_def), optional, intent(out) :: edge_cells_y

    integer(i_def), optional, intent(out) :: void_cell
    real(r_def),    optional, intent(out) :: north_pole(2)
    real(r_def),    optional, intent(out) :: null_island(2)
    real(r_def),    optional, intent(out) :: equatorial_latitude

  end subroutine get_metadata_interface


  !-----------------------------------------------------------------------------
  !> @brief Interface: Returns mesh dimension information.
  !>
  !> @param[in]     self                   The generator strategy object.
  !> @param[out]    num_nodes              Number of nodes
  !> @param[out]    num_edges              Number of edges
  !> @param[out]    num_faces              Number of faces
  !> @param[out]    num_nodes_per_face     Number of nodes per face
  !> @param[out]    num_edges_per_face     Number of edges per face
  !> @param[out]    num_nodes_per_edge     Number of nodes per edge
  !> @param[out]    max_num_faces_per_node Maximum number of faces surrounding
  !>                                       each node
  !-----------------------------------------------------------------------------
  subroutine get_dimensions_interface (self, num_nodes, num_edges, num_faces, &
                 num_nodes_per_face, num_edges_per_face, num_nodes_per_edge,  &
                 max_num_faces_per_node )

    import :: ugrid_generator_type, i_def

    implicit none

    class(ugrid_generator_type), intent(in) :: self

    integer(i_def), intent(out) :: num_nodes
    integer(i_def), intent(out) :: num_edges
    integer(i_def), intent(out) :: num_faces
    integer(i_def), intent(out) :: num_nodes_per_face
    integer(i_def), intent(out) :: num_edges_per_face
    integer(i_def), intent(out) :: num_nodes_per_edge
    integer(i_def), intent(out) :: max_num_faces_per_node

  end subroutine get_dimensions_interface


  !-----------------------------------------------------------------------------
  !> @brief Interface: Gets coordinates of nodes, edges and faces.
  !> @param[in]   self              The generator strategy object.
  !> @param[out]  node_coordinates  Node coordinates
  !> @param[out]  cell_coordinates  Cell coordinates
  !> @param[out]  domain_extents    Principal coordinates that
  !>                                describe the domain shape.
  !> @param[out]  coord_units_x     Units for x-coordinate
  !> @param[out]  coord_units_y     Units for y-coordinate
  !-----------------------------------------------------------------------------
  subroutine get_coordinates_interface (self, node_coordinates, &
                                              cell_coordinates, &
                                              domain_extents,   &
                                              coord_units_x,    &
                                              coord_units_y)

    import :: ugrid_generator_type, r_def, str_def

    implicit none

    class(ugrid_generator_type), intent(in) :: self

    real(r_def), intent(out)        :: node_coordinates(:,:)
    real(r_def), intent(out)        :: cell_coordinates(:,:)
    real(r_def), intent(out)        :: domain_extents(:,:)
    character(str_def), intent(out) :: coord_units_x
    character(str_def), intent(out) :: coord_units_y

  end subroutine get_coordinates_interface


  !-----------------------------------------------------------------------------
  !> @brief Interface: Gets a selection of connectivity information from the
  !>                   mesh generator.
  !>
  !> @param[out]    face_node_connectivity Nodes around each face
  !> @param[out]    face_edge_connectivity Edges around each face.
  !> @param[out]    face_face_connectivity Faces adjacent to each face.
  !> @param[out]    edge_node_connectivity Nodes defining each edge
  !-----------------------------------------------------------------------------
  subroutine get_connectivity_interface ( self,                        &
                       face_node_connectivity, face_edge_connectivity, &
                       face_face_connectivity, edge_node_connectivity )

    import :: ugrid_generator_type, i_def

    implicit none

    class(ugrid_generator_type), intent(in) :: self

    integer(i_def), intent(out) :: face_node_connectivity(:,:)
    integer(i_def), intent(out) :: face_edge_connectivity(:,:)
    integer(i_def), intent(out) :: face_face_connectivity(:,:)
    integer(i_def), intent(out) :: edge_node_connectivity(:,:)

  end subroutine get_connectivity_interface


end interface

end module ugrid_generator_mod
