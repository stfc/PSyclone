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

use constants_mod, only : r_def, i_def, str_def, str_long, l_def

implicit none

private

!-------------------------------------------------------------------------------
!> @brief Abstract generator type
!-------------------------------------------------------------------------------
type, abstract, public :: ugrid_generator_type
  private

contains
  procedure ( generate_interface         ),     deferred :: generate
  procedure ( get_metadata_interface     ),     deferred :: get_metadata
  procedure ( get_dimensions_interface   ),     deferred :: get_dimensions
  procedure ( get_coordinates_interface  ),     deferred :: get_coordinates
  procedure ( get_connectivity_interface ),     deferred :: get_connectivity
end type ugrid_generator_type

!-------------------------------------------------------------------------------
! Abstract interfaces
!-------------------------------------------------------------------------------

abstract interface

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
  !> @param[in]             self               The generator strategy object.
  !> @param[out, optional]  mesh_name          Name of mesh instance to generate
  !> @param[out, optional]  mesh_class         Primitive shape, i.e. sphere, plane
  !> @param[out, optional]  periodic_x         Periodic in E-W direction.
  !> @param[out, optional]  periodic_y         Periodic in N-S direction.
  !> @param[out, optional]  npanels            Number of panels use to describe mesh
  !> @param[out, optional]  edge_cells_x       Number of panel edge cells (x-axis).
  !> @param[out, optional]  edge_cells_y       Number of panel edge cells (y-axis).
  !> @param[out, optional]  constructor_inputs Inputs used to create this mesh from
  !>                                           the this ugrid_generator_type
  !> @param[out, optional]  nmaps              Number of maps to create with this mesh
  !>                                           as source mesh
  !> @param[out, optional]  target_mesh_names  Mesh names of the target meshes that
  !>                                           this mesh has maps for.
  !> @param[out, optional]  maps_edge_cells_x  Number of panel edge cells (x-axis) of
  !>                                           target mesh(es) to create map(s) for.
  !> @param[out, optional]  maps_edge_cells_y  Number of panel edge cells (y-axis) of
  !>                                           target mesh(es) to create map(s) for.
  !-----------------------------------------------------------------------------
  subroutine get_metadata_interface ( self, mesh_name, mesh_class,          &
                                      periodic_x, periodic_y, npanels,      &
                                      edge_cells_x, edge_cells_y,           &
                                      constructor_inputs, nmaps,            &
                                      target_mesh_names,                      &
                                      maps_edge_cells_x, maps_edge_cells_y )

    import :: ugrid_generator_type, i_def, str_def, str_long, l_def

    implicit none

    class(ugrid_generator_type),  intent(in)  :: self
    character(str_def), optional, intent(out) :: mesh_name
    character(str_def), optional, intent(out) :: mesh_class
    logical(l_def),     optional, intent(out) :: periodic_x
    logical(l_def),     optional, intent(out) :: periodic_y
    character(str_long),optional, intent(out) :: constructor_inputs
    character(str_def), allocatable, &
                        optional, intent(out) :: target_mesh_names(:)

    integer(i_def), allocatable, &
                    optional, intent(out) :: maps_edge_cells_x(:)
    integer(i_def), allocatable, &
                    optional, intent(out) :: maps_edge_cells_y(:)
    integer(i_def), optional, intent(out) :: npanels
    integer(i_def), optional, intent(out) :: nmaps
    integer(i_def), optional, intent(out) :: edge_cells_x
    integer(i_def), optional, intent(out) :: edge_cells_y

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
  !-----------------------------------------------------------------------------
  subroutine get_dimensions_interface (self, num_nodes, num_edges, num_faces,  &
                 num_nodes_per_face, num_edges_per_face, num_nodes_per_edge)

    import :: ugrid_generator_type, i_def

    implicit none

    class(ugrid_generator_type), intent(in) :: self

    integer(i_def), intent(out) :: num_nodes
    integer(i_def), intent(out) :: num_edges
    integer(i_def), intent(out) :: num_faces
    integer(i_def), intent(out) :: num_nodes_per_face
    integer(i_def), intent(out) :: num_edges_per_face
    integer(i_def), intent(out) :: num_nodes_per_edge

  end subroutine get_dimensions_interface


  !-----------------------------------------------------------------------------
  !> @brief Interface: Gets coordinates of nodes, edges and faces.
  !> @param[in]   self              The generator strategy object.
  !> @param[out]  node_coordinates  Node coordinates
  !> @param[out]  cell_coordinates  Cell coordinates
  !> @param[out]  coord_units_x     Units for x-coordinate
  !> @param[out]  coord_units_y     Units for y-coordinate
  !-----------------------------------------------------------------------------
  subroutine get_coordinates_interface (self, node_coordinates, &
                                              cell_coordinates, &
                                              coord_units_x,    &
                                              coord_units_y)

    import :: ugrid_generator_type, r_def, str_def

    implicit none

    class(ugrid_generator_type), intent(in) :: self

    real(r_def), intent(out)        :: node_coordinates(:,:)
    real(r_def), intent(out)        :: cell_coordinates(:,:)
    character(str_def), intent(out) :: coord_units_x
    character(str_def), intent(out) :: coord_units_y

  end subroutine get_coordinates_interface


  !-----------------------------------------------------------------------------
  !> @brief Interface: Gets a selection of connectivity information from the
  !>                   mesh generator.
  !> @param[in]     self                   The generator strategy object.
  !> @param[out]    face_node_connectivity Nodes around each face
  !> @param[out]    edge_node_connectivity Nodes defining each edge
  !> @param[out]    face_edge_connectivity Edges around each face.
  !> @param[out]    face_face_connectivity Faces adjacent to each face.
  !-----------------------------------------------------------------------------
  subroutine get_connectivity_interface (self,                         &
                       face_node_connectivity, edge_node_connectivity, &
                       face_edge_connectivity, face_face_connectivity)

    import :: ugrid_generator_type, i_def

    implicit none

    class(ugrid_generator_type), intent(in) :: self

    integer(i_def), intent(out) :: face_node_connectivity(:,:)
    integer(i_def), intent(out) :: edge_node_connectivity(:,:)
    integer(i_def), intent(out) :: face_edge_connectivity(:,:)
    integer(i_def), intent(out) :: face_face_connectivity(:,:)

  end subroutine get_connectivity_interface


end interface

end module ugrid_generator_mod

