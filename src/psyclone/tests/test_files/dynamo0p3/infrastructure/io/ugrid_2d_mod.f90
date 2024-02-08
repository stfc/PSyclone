

!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

! BSD 3-Clause License
!
! Copyright (c) 2020-2024, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Modified by J. Henrichs, Bureau of Meteorology

!> @brief   Store 2-dimensional ugrid mesh data.
!> @details Holds all information necessary to define ugrid vn0.9 compliant
!>          storage of 2-dimensional meshes. Pulling data out is currently
!>          done with accessor routines; this may change as dynamo matures.
!-------------------------------------------------------------------------------

module ugrid_2d_mod

use constants_mod,  only : i_def, r_def, str_def, str_long, l_def
use ugrid_file_mod, only : ugrid_file_type

implicit none

private

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------

integer(i_def), parameter :: TOPOLOGY_DIMENSION  = 2

!-------------------------------------------------------------------------------
!> @brief Stores 2-dimensional grid information
!-------------------------------------------------------------------------------
type, public :: ugrid_2d_type
  private

  character(str_def)  :: mesh_name
  character(str_def)  :: mesh_class           !< Primitive class of mesh,
                                              !< i.e. sphere, plane
  logical(l_def)      :: periodic_x = .false. !< Periodic in E-W direction.
  logical(l_def)      :: periodic_y = .false. !< Periodic in N-S direction.
  character(str_long) :: constructor_inputs   !< Inputs used to generate mesh

  character(str_def)  :: coord_units_x
  character(str_def)  :: coord_units_y

  integer(i_def) :: edge_cells_x !< Number of cells on panel edge x-axis
  integer(i_def) :: edge_cells_y !< Number of cells on panel edge y-axis

  ! Numbers of different entities
  integer(i_def) :: num_cells                !< Number of cells
  integer(i_def) :: num_nodes                !< Number of nodes
  integer(i_def) :: num_edges                !< Number of edges
  integer(i_def) :: num_faces                !< Number of faces

  integer(i_def) :: num_nodes_per_face       !< Number of nodes surrounding each face
  integer(i_def) :: num_nodes_per_edge       !< Number of nodes defining each edge
  integer(i_def) :: num_edges_per_face       !< Number of edges bordering each face
  integer(i_def) :: max_num_faces_per_node   !< Maximum number of faces surrounding each node


  ! Coordinates
  real(r_def), allocatable :: node_coordinates(:,:) !< Coordinates of nodes
  real(r_def), allocatable :: face_coordinates(:,:) !< Coordinates of faces

  ! Connectivity
  integer(i_def), allocatable :: face_node_connectivity(:,:) !< Nodes belonging to each face
  integer(i_def), allocatable :: edge_node_connectivity(:,:) !< Nodes belonging to each edge
  integer(i_def), allocatable :: face_edge_connectivity(:,:) !< Edges belonging to each face
  integer(i_def), allocatable :: face_face_connectivity(:,:) !< Neighbouring faces of each face

  ! Target mesh map variables
  integer(i_def) :: nmaps = 0 !< Number of mesh maps for this mesh (as source)

  character(str_def), allocatable :: target_mesh_names(:)   !< Target mesh names
  integer(i_def),     allocatable :: target_edge_cells_x(:) !< Target meshes panel edge cells in x-axis
  integer(i_def),     allocatable :: target_edge_cells_y(:) !< Target meshes panel edge cells in y-axis

  ! File handler
  class(ugrid_file_type), allocatable :: file_handler

contains
  procedure :: get_n_meshes
  procedure :: get_mesh_names
  procedure :: get_dimensions
  procedure :: set_by_generator
  procedure :: set_file_handler
  procedure :: set_from_file_read
  procedure :: get_metadata
  procedure :: get_coord_units
  procedure :: get_node_coords
  procedure :: get_face_coords
  procedure :: get_node_coords_transpose
  procedure :: get_face_node_connectivity
  procedure :: get_face_edge_connectivity
  procedure :: get_face_face_connectivity
  procedure :: get_face_node_connectivity_transpose
  procedure :: get_face_edge_connectivity_transpose
  procedure :: get_face_face_connectivity_transpose
  procedure :: write_coordinates

  !> Routine to destroy object
  procedure :: clear

  !> Object finalizer
  final     :: ugrid_2d_destructor

end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

!-------------------------------------------------------------------------------
!>  @brief Gets number of nodes, edges, faces etc.
!>
!>  @param[in]      self                    Calling ugrid object.
!>  @param[out]     num_nodes               Number of nodes
!>  @param[out]     num_edges               Number of edges
!>  @param[out]     num_faces               Number of faces
!>  @param[out]     num_nodes_per_face      Number of nodes around each face.
!>  @param[out]     num_edges_per_face      Number of edges around each face.
!>  @param[out]     num_nodes_per_edge      Number of nodes defining each edge.
!>  @param[out]     max_num_faces_per_node  Maximum number of faces surrounding each node.
!-------------------------------------------------------------------------------

subroutine get_dimensions( self, num_nodes, num_edges, num_faces,  &
                           num_nodes_per_face, num_edges_per_face, &
                           num_nodes_per_edge, max_num_faces_per_node )

  implicit none

  ! Arguments
  class(ugrid_2d_type), intent(in) :: self

  integer(i_def), intent(out) :: num_nodes
  integer(i_def), intent(out) :: num_edges
  integer(i_def), intent(out) :: num_faces
  integer(i_def), intent(out) :: num_nodes_per_face
  integer(i_def), intent(out) :: num_edges_per_face
  integer(i_def), intent(out) :: num_nodes_per_edge
  integer(i_def), intent(out) :: max_num_faces_per_node

  num_nodes = self%num_nodes
  num_edges = self%num_edges
  num_faces = self%num_faces

  num_nodes_per_face = self%num_nodes_per_face
  num_edges_per_face = self%num_edges_per_face
  num_nodes_per_edge = self%num_nodes_per_edge
  max_num_faces_per_node = self%max_num_faces_per_node

  return
end subroutine get_dimensions

!-------------------------------------------------------------------------------
!> @brief Gets a list of the mesh names in a ugrid file
!>
!> @param[in]  self       Calling ugrid object
!> @param[in]  filename   The name of the file to query
!> @param[out] mesh_names Character[:], Name of mesh topologies in the file.
!-------------------------------------------------------------------------------
subroutine get_mesh_names(self, filename, mesh_names)

  implicit none

  class(ugrid_2d_type), intent(inout) :: self
  character(len=*),     intent(in)    :: filename
  character(len=*),     intent(out)   :: mesh_names(:)

  call self%file_handler%file_open(trim(filename))
  call self%file_handler%get_mesh_names(mesh_names)
  call self%file_handler%file_close()

  return
end subroutine get_mesh_names

!-------------------------------------------------------------------------------
!> @brief Gets the number of the mesh topologies in a ugrid file
!>
!> @param[in]  self       Calling ugrid object
!> @param[in]  filename   The name of the file to query
!> @param[out] n_meshes   Integer, Number of mesh topologies in the file.
!-------------------------------------------------------------------------------
subroutine get_n_meshes(self, filename, n_meshes)

  implicit none

  class(ugrid_2d_type), intent(inout) :: self
  character(len=*),     intent(in)    :: filename
  integer(i_def),       intent(out)   :: n_meshes

  call self%file_handler%file_open(trim(filename))
  n_meshes = self%file_handler%get_n_meshes()
  call self%file_handler%file_close()

  return
end subroutine get_n_meshes

!-------------------------------------------------------------------------------
!>  @brief   Allocates ugrid_2d internal storage, populated by ugrid generator.
!>  @details Allocates component arrays according to sizes obtained from the
!>           passed generator strategy.
!>
!>  @param[in,out] self               The ugrid object for which to
!>                                    allocate storage.
!>  @param[in]     generator_strategy The generator strategy in use.
!-------------------------------------------------------------------------------

subroutine allocate_arrays(self, generator_strategy)
  use ugrid_generator_mod, only: ugrid_generator_type
  implicit none

  ! Arguments
  type(ugrid_2d_type),         intent(inout) :: self
  class(ugrid_generator_type), intent(in)    :: generator_strategy

  call generator_strategy%get_dimensions(                &
         num_nodes          = self%num_nodes,            &
         num_edges          = self%num_edges,            &
         num_faces          = self%num_faces,            &
         num_nodes_per_face = self%num_nodes_per_face,   &
         num_edges_per_face = self%num_edges_per_face,   &
         num_nodes_per_edge = self%num_nodes_per_edge)



  if ( allocated( self%node_coordinates ) )       &
                                     deallocate( self%node_coordinates )
  if ( allocated( self%face_coordinates ) )       &
                                     deallocate( self%face_coordinates )

  if ( allocated( self%edge_node_connectivity ) ) &
                                     deallocate( self%edge_node_connectivity )

  if ( allocated( self%face_node_connectivity ) ) &
                                     deallocate( self%face_node_connectivity )

  if ( allocated( self%face_edge_connectivity ) ) &
                                     deallocate( self%face_edge_connectivity )

  if ( allocated( self%face_face_connectivity ) ) &
                                     deallocate( self%face_face_connectivity )

  allocate(self%node_coordinates(2, self%num_nodes))
  allocate(self%face_coordinates(2, self%num_faces))
  allocate(self%edge_node_connectivity(self%num_nodes_per_edge, self%num_edges))
  allocate(self%face_node_connectivity(self%num_nodes_per_face, self%num_faces))
  allocate(self%face_edge_connectivity(self%num_edges_per_face, self%num_faces))
  allocate(self%face_face_connectivity(self%num_edges_per_face, self%num_faces))

  if (self%nmaps > 0) then
    allocate(self%target_mesh_names(self%nmaps))
    allocate(self%target_edge_cells_x(self%nmaps))
    allocate(self%target_edge_cells_y(self%nmaps))
  end if

  return
end subroutine allocate_arrays

!-------------------------------------------------------------------------------
!>  @brief   Allocates ugrid_2d internal storage, populated by ugrid file.
!>  @details Allocates component arrays according to sizes already stored in
!>           the ugrid_2d object. These arrays are populated elsewhere
!>           by a ugrid file strategy.
!>
!>  @param[in,out] self    The ugrid object for which to allocate storage.
!-------------------------------------------------------------------------------

subroutine allocate_arrays_for_file(self)
  implicit none

  ! Arguments
  type(ugrid_2d_type),    intent(inout) :: self

  if ( allocated( self%node_coordinates ) )       &
                                     deallocate( self%node_coordinates )
  if ( allocated( self%face_coordinates ) )       &
                                     deallocate( self%face_coordinates )

  if ( allocated( self%edge_node_connectivity ) ) &
                                     deallocate( self%edge_node_connectivity )

  if ( allocated( self%face_node_connectivity ) ) &
                                     deallocate( self%face_node_connectivity )

  if ( allocated( self%face_edge_connectivity ) ) &
                                     deallocate( self%face_edge_connectivity )

  if ( allocated( self%face_face_connectivity ) ) &
                                     deallocate( self%face_face_connectivity )

  if ( allocated( self%target_mesh_names ) ) &
                                     deallocate( self%target_mesh_names )

  allocate(self%node_coordinates(2, self%num_nodes))
  allocate(self%face_coordinates(2, self%num_faces))

  allocate(self%edge_node_connectivity(self%num_nodes_per_edge, self%num_edges))
  allocate(self%face_node_connectivity(self%num_nodes_per_face, self%num_faces))
  allocate(self%face_edge_connectivity(self%num_edges_per_face, self%num_faces))
  allocate(self%face_face_connectivity(self%num_edges_per_face, self%num_faces))

  if (self%nmaps > 0) then
    allocate( self%target_mesh_names(self%nmaps) )
  end if

  return
end subroutine allocate_arrays_for_file

!---------------------------------------------------------------------------------
!>  @brief   Populate arrays according to passed generator strategy.
!>  @details Calls back to the passed generator strategy in order to populate
!>           the coordinate and connectivity arrays.
!>
!>  @param[in,out] self               Calling ugrid object.
!>  @param[in,out] generator_strategy The generator with which to generate the mesh.
!---------------------------------------------------------------------------------

subroutine set_by_generator(self, generator_strategy)
  use ugrid_generator_mod, only: ugrid_generator_type
  implicit none

  class(ugrid_2d_type),        intent(inout) :: self
  class(ugrid_generator_type), intent(inout) :: generator_strategy

  call generator_strategy%get_metadata                &
      ( mesh_name          = self%mesh_name,          &
        mesh_class         = self%mesh_class,         &
        periodic_x         = self%periodic_x,         &
        periodic_y         = self%periodic_y,         &
        edge_cells_x       = self%edge_cells_x,       &
        edge_cells_y       = self%edge_cells_y,       &
        constructor_inputs = self%constructor_inputs, &
        nmaps              = self%nmaps )

  call generator_strategy%generate()

  call allocate_arrays(self, generator_strategy)

  if (self%nmaps > 0) then
    call generator_strategy%get_metadata                &
        ( target_mesh_names = self%target_mesh_names,   &
          maps_edge_cells_x = self%target_edge_cells_x, &
          maps_edge_cells_y = self%target_edge_cells_y )
  end if

  call generator_strategy%get_coordinates         &
      ( node_coordinates = self%node_coordinates, &
        cell_coordinates = self%face_coordinates, &
        coord_units_x    = self%coord_units_x,    &
        coord_units_y    = self%coord_units_y )

  call generator_strategy%get_connectivity                    &
      ( face_node_connectivity = self%face_node_connectivity, &
        edge_node_connectivity = self%edge_node_connectivity, &
        face_edge_connectivity = self%face_edge_connectivity, &
        face_face_connectivity = self%face_face_connectivity )

  return
end subroutine set_by_generator

!-------------------------------------------------------------------------------
!> @brief   Sets the file read/write strategy.
!> @details Receives a file-handler object and moves its allocation into
!>          the appropriate type component. On exit, the file_handler
!>          dummy argument will no longer be allocated.
!>
!> @param[in,out] self         Calling ugrid object.
!> @param[in,out] file_handler The file handler to use for IO.
!-------------------------------------------------------------------------------

subroutine set_file_handler(self, file_handler)
  implicit none

  ! Arguments
  class(ugrid_2d_type),                intent(inout) :: self
  class(ugrid_file_type), allocatable, intent(inout) :: file_handler

  call move_alloc(file_handler, self%file_handler)

  return
end subroutine set_file_handler

!-------------------------------------------------------------------------------
!> @brief   Reads ugrid information and populates internal arrays.
!> @details Calls back to the file handler strategy (component) in order to
!>          read the ugrid mesh data and populate internal arrays with data
!>          from a file.
!>
!> @param[in,out] self      Calling ugrid object.
!> @param[in]     mesh_name Name of the mesh topology
!> @param[in]     filename  Name of the ugrid file
!-------------------------------------------------------------------------------

subroutine set_from_file_read(self, mesh_name, filename)
  implicit none

  ! Arguments
  class(ugrid_2d_type), intent(inout) :: self
  character(len=*),     intent(in)    :: mesh_name
  character(len=*),     intent(in)    :: filename

  self%mesh_name = trim(mesh_name)

  call self%file_handler%file_open(trim(filename))

  call self%file_handler%get_dimensions(                    &
         mesh_name              = self%mesh_name,           &
         num_nodes              = self%num_nodes,           &
         num_edges              = self%num_edges,           &
         num_faces              = self%num_faces,           &
         num_nodes_per_face     = self%num_nodes_per_face,  &
         num_edges_per_face     = self%num_edges_per_face,  &
         num_nodes_per_edge     = self%num_nodes_per_edge,  &
         max_num_faces_per_node = self%max_num_faces_per_node )

  call allocate_arrays_for_file(self)

  call self%file_handler%read_mesh(                         &
      mesh_name              = self%mesh_name,              &
      mesh_class             = self%mesh_class,             &
      periodic_x             = self%periodic_x,             &
      periodic_y             = self%periodic_y,             &
      constructor_inputs     = self%constructor_inputs,     &
      node_coordinates       = self%node_coordinates,       &
      face_coordinates       = self%face_coordinates,       &
      coord_units_x          = self%coord_units_x,          &
      coord_units_y          = self%coord_units_y,          &
      face_node_connectivity = self%face_node_connectivity, &
      edge_node_connectivity = self%edge_node_connectivity, &
      face_edge_connectivity = self%face_edge_connectivity, &
      face_face_connectivity = self%face_face_connectivity, &
      num_targets            = self%nmaps,                  &
      target_mesh_names      = self%target_mesh_names )

  call self%file_handler%file_close()

  return
end subroutine set_from_file_read


!-------------------------------------------------------------------------------
!> @brief   Gets metadata of the current mesh in this object which was
!>          set by the ugrid_generator_type or read in from NetCDF (UGRID) file.
!>
!> @param[in]            self               The calling ugrid object.
!> @param[out, optional] mesh_name          Name of the current mesh topology.
!> @param[out, optional] mesh_class         Primitive class of the mesh topology.
!> @param[out, optional] periodic_x         Periodic in E-W direction.
!> @param[out, optioanl] periodic_y         Periodic in N-S direction.
!> @param[out, optional] edge_cells_x       Number of panel edge cells (x-axis).
!> @param[out, optional] edge_cells_y       Number of panel edge cells (y-axis).
!> @param[out, optional] constructor_inputs Input arguments use to create this mesh.
!> @param[out, optional] target_mesh_names  Names of target mesh topologies in this file
!>                                          which this mesh possesses cell-cell maps for.
!-------------------------------------------------------------------------------
subroutine get_metadata( self, mesh_name, mesh_class,         &
                         periodic_x, periodic_y,              &
                         edge_cells_x, edge_cells_y,          &
                         constructor_inputs, nmaps,           &
                         target_mesh_names )

  implicit none

  class(ugrid_2d_type), intent(in) :: self
  character(str_def),   optional, intent(out) :: mesh_name
  character(str_def),   optional, intent(out) :: mesh_class
  logical(l_def),       optional, intent(out) :: periodic_x
  logical(l_def),       optional, intent(out) :: periodic_y
  integer(i_def),       optional, intent(out) :: edge_cells_x
  integer(i_def),       optional, intent(out) :: edge_cells_y
  character(str_long),  optional, intent(out) :: constructor_inputs
  integer(i_def),       optional, intent(out) :: nmaps
  character(str_def),   optional, intent(out), &
                                  allocatable :: target_mesh_names(:)

  if (present(mesh_name))          mesh_name          = self%mesh_name
  if (present(mesh_class))         mesh_class         = self%mesh_class
  if (present(periodic_x))         periodic_x         = self%periodic_x
  if (present(periodic_y))         periodic_y         = self%periodic_y
  if (present(constructor_inputs)) constructor_inputs = self%constructor_inputs
  if (present(edge_cells_x))       edge_cells_x       = self%edge_cells_x
  if (present(edge_cells_y))       edge_cells_y       = self%edge_cells_y
  if (present(nmaps))              nmaps              = self%nmaps
  if (self%nmaps > 0 .and. present(target_mesh_names)) then
    target_mesh_names = self%target_mesh_names
  end if

end subroutine get_metadata

!-------------------------------------------------------------------------------
!> @brief   Gets node coordinate units in [x,y] or [longitude, latitude] directions
!>
!> @param[in]   self          The calling ugrid object.
!> @param[out]  coord_units_x String for coordinate units in x-direction
!> @param[out]  coord_units_y String for coordinate units in y-direction
!-------------------------------------------------------------------------------
subroutine get_coord_units(self, coord_units_x, coord_units_y)
  implicit none

  class(ugrid_2d_type), intent(in)  :: self
  character(str_def),   intent(out) :: coord_units_x
  character(str_def),   intent(out) :: coord_units_y

  coord_units_x = self%coord_units_x
  coord_units_y = self%coord_units_y

  return
end subroutine get_coord_units

!-------------------------------------------------------------------------------
!> @brief   Gets node coordinates with ugrid array index ordering.
!> @details Returns a rank-two array of node coordinates, with the
!>          coordinate dimension index innermost, and the node number
!>          outermost. Format: [long, lat, radius].
!>
!> @param[in]   self         The calling ugrid object.
!> @param[out]  node_coords  Node coordinate array.
!-------------------------------------------------------------------------------

subroutine get_node_coords(self, node_coords)
  implicit none

  class(ugrid_2d_type), intent(in)  :: self

  real(r_def),          intent(out) :: node_coords(:,:)

  integer(i_def) :: i

  do i = 1, self%num_nodes
    node_coords(1:2,i) = self%node_coordinates(1:2,i)
  end do

  return
end subroutine get_node_coords


!-------------------------------------------------------------------------------
!> @brief   Gets face coordinates with ugrid array index ordering.
!> @details Returns a rank-two array of node coordinates, with the
!>          coordinate dimension index innermost, and the face number
!>          outermost. Format: [long, lat].
!>
!> @param[in]   self        The calling ugrid object.
!> @return      face_coords Face coordinate array.
!-------------------------------------------------------------------------------

function get_face_coords(self) result(face_coords)

  implicit none

  class(ugrid_2d_type), intent(in)  :: self

  real(r_def), allocatable :: face_coords(:,:)

  face_coords = self%face_coordinates(:,:)

  return
end function get_face_coords


!-------------------------------------------------------------------------------
!> @brief   Gets node coordinates with transposed ugrid array index ordering.
!> @details Returns a rank-two array of node coordinates, with the
!>          coordinate dimension index outermost, and the node number
!>          innermost. This is the transpose of the ugrid index ordering.
!>          Format: [long, lat, radius].
!>
!> @param[in]   self        Calling ugrid object.
!> @param[out]  node_coords Node coordinate array.
!-------------------------------------------------------------------------------

subroutine get_node_coords_transpose(self, node_coords)
  implicit none

  class(ugrid_2d_type), intent(in)  :: self
  real(r_def),          intent(out) :: node_coords(:,:)

  integer(i_def) :: i

  do i = 1, self%num_nodes
    node_coords(i,1:2) = self%node_coordinates(1:2,i)
  end do

  return
end subroutine get_node_coords_transpose

!-------------------------------------------------------------------------------
!> @brief   Gets an array of node indices surrounding each face.
!> @details Returns a rank-two array of nodes surrounding each face, with
!>          the nodes surrounding any single face being contiguous.
!>
!> @param[in]   self                   Calling ugrid object
!> @param[out]  face_node_connectivity Indices of nodes adjacent to faces.
!-------------------------------------------------------------------------------

subroutine get_face_node_connectivity(self, face_node_connectivity)
  implicit none

  class(ugrid_2d_type), intent(in)  :: self
  integer(i_def),       intent(out) :: face_node_connectivity(:,:)

  integer(i_def) :: i,j

  do j = 1, self%num_faces
    do i = 1, self%num_nodes_per_face
      face_node_connectivity(i,j) = self%face_node_connectivity(i,j)
    end do
  end do

  return
end subroutine get_face_node_connectivity

!-------------------------------------------------------------------------------
!> @brief   Gets an array of node indices surrounding each face with transposed
!>          ugrid index ordering.
!> @details Returns a rank-two array of nodes surrounding each face, with the
!>          face indices being contiguous and the nodes surrounding any single
!>          face being non-contiguous. This is the transpose of the ugrid
!>          index ordering. This transpose routine is needed to interface
!>          with the current Dynamo index ordering.
!>
!> @param[in]    self                   Calling ugrid object
!> @param[out]   face_node_connectivity Indices of nodes adjacent to faces.
!-------------------------------------------------------------------------------

subroutine get_face_node_connectivity_transpose(self, face_node_connectivity)
  implicit none

  class(ugrid_2d_type), intent(in)  :: self
  integer(i_def),       intent(out) :: face_node_connectivity(:,:)

  integer(i_def) :: i,j

  do j = 1, self%num_faces
    do i = 1, self%num_nodes_per_face
      face_node_connectivity(j,i) = self%face_node_connectivity(i,j)
    end do
  end do

  return
end subroutine get_face_node_connectivity_transpose

!-------------------------------------------------------------------------------
!> @brief   Gets an array of edge indices surrounding each face.
!> @details Returns a rank-two array of edges surrounding each face, with
!>          the edges surrounding any single face being contiguous.
!>
!> @param[in]  self                   Calling ugrid object
!> @param[out] face_edge_connectivity Indices of edges adjacent to faces.
!-------------------------------------------------------------------------------

subroutine get_face_edge_connectivity(self, face_edge_connectivity)
  implicit none

  class(ugrid_2d_type), intent(in)  :: self
  integer(i_def),       intent(out) :: face_edge_connectivity(:,:)

  integer(i_def) :: i,j

  do j = 1, self%num_faces
    do i = 1, self%num_edges_per_face
      face_edge_connectivity(i,j) = self%face_edge_connectivity(i,j)
    end do
  end do

  return
end subroutine get_face_edge_connectivity

!-------------------------------------------------------------------------------
!> @brief   Gets an array of edge indices surrounding each face with transposed
!>          ugrid index ordering.
!> @details Returns a rank-two array of edges surrounding each face, with the
!>          face indices being contiguous and the edges surrounding any single
!>          face being non-contiguous. This is the transpose of the ugrid
!>          index ordering. This transpose routine is needed to interface
!>          with the current Dynamo index ordering.
!>
!> @param[in]    self                   Calling ugrid object
!> @param[out]   face_edge_connectivity Indices of edges adjacent to faces.
!-------------------------------------------------------------------------------

subroutine get_face_edge_connectivity_transpose(self, face_edge_connectivity)
  implicit none

  class(ugrid_2d_type), intent(in)  :: self
  integer(i_def),       intent(out) :: face_edge_connectivity(:,:)

  integer(i_def) :: i,j

  do j = 1, self%num_faces
    do i = 1, self%num_edges_per_face
      face_edge_connectivity(j,i) = self%face_edge_connectivity(i,j)
    end do
  end do

  return
end subroutine get_face_edge_connectivity_transpose

!-------------------------------------------------------------------------------
!> @brief   Gets an array of face indices surrounding each face.
!> @details Returns a rank-two array of faces surrounding each face, with
!>          the faces surrounding any single face being contiguous.
!>
!> @param[in] self                   Calling ugrid object
!> @param[in] face_face_connectivity Indices of faces adjacent to faces.
!-------------------------------------------------------------------------------

subroutine get_face_face_connectivity(self, face_face_connectivity)
  implicit none

  class(ugrid_2d_type), intent(in)  :: self
  integer(i_def),       intent(out) :: face_face_connectivity(:,:)

  integer(i_def) :: i,j

  do j = 1, self%num_faces
    do i = 1, self%num_nodes_per_face
      face_face_connectivity(i,j) = self%face_face_connectivity(i,j)
    end do
  end do

  return
end subroutine get_face_face_connectivity

!-------------------------------------------------------------------------------
!> @brief   Gets an array of face indices surrounding each face with transposed
!>          ugrid index ordering.
!> @details Returns a rank-two array of faces surrounding each face, with the
!>          face indices being contiguous and the the faces surrounding any
!>          single face being non-contiguous. This transpose routine is needed
!>          to interface with the current Dynamo index ordering.
!>
!> @param[in]  self                   Calling ugrid object
!> @param[out] face_face_connectivity Indices of faces adjacent to faces.
!-------------------------------------------------------------------------------

subroutine get_face_face_connectivity_transpose(self, face_face_connectivity)
  implicit none

  class(ugrid_2d_type), intent(in)  :: self
  integer(i_def),       intent(out) :: face_face_connectivity(:,:)

  integer(i_def) :: i,j

  do j = 1, self%num_faces
    do i = 1, self%num_nodes_per_face
      face_face_connectivity(j,i) = self%face_face_connectivity(i,j)
    end do
  end do

  return
end subroutine get_face_face_connectivity_transpose

!-------------------------------------------------------------------------------
!> @brief   Writes coordinates to a .dat file in the units they are held in
!>          within the UGRID file.
!> @details Produces a file with rough output of coordinates. Intended to be
!>          a temporary routine only: would be better implemented for the
!>          long-term as a plain-text ugrid file strategy. Hence the
!>          good-enough-for-now hardwired unit numbers and file names.
!>
!> @param[in] self  The ugrid object.
!-------------------------------------------------------------------------------

subroutine write_coordinates(self)
  implicit none

  ! Arguments
  class(ugrid_2d_type), intent(in) :: self

  integer(i_def) :: inode

  open(56, file='nodes.dat')
  do inode = 1, self%num_nodes
    write(56,*) self%node_coordinates(:,inode)
  end do
  close(56)

  return
end subroutine write_coordinates

!-------------------------------------------------------------------------------
!> @brief Routine to destroy object.
!> @param[in] self, The calling ugrid_2d_type
subroutine clear(self)

  implicit none

  class (ugrid_2d_type), intent(inout) :: self

  if (allocated(self%node_coordinates))       deallocate( self%node_coordinates )
  if (allocated(self%face_coordinates))       deallocate( self%face_coordinates )
  if (allocated(self%face_node_connectivity)) deallocate( self%face_node_connectivity )
  if (allocated(self%edge_node_connectivity)) deallocate( self%edge_node_connectivity )
  if (allocated(self%face_edge_connectivity)) deallocate( self%face_edge_connectivity )
  if (allocated(self%face_face_connectivity)) deallocate( self%face_face_connectivity )
  if (allocated(self%target_mesh_names))      deallocate( self%target_mesh_names )

  if (allocated(self%target_edge_cells_x))    deallocate( self%target_edge_cells_x )
  if (allocated(self%target_edge_cells_y))    deallocate( self%target_edge_cells_y )
  if (allocated(self%file_handler))           deallocate( self%file_handler )

end subroutine clear

!-------------------------------------------------------------------------------
!> @brief Finalizer routine which should automatically call clear
!>        when object is out of scope.
!> @param[in] self, The calling ugrid_2d instance
subroutine ugrid_2d_destructor(self)

  implicit none

  type (ugrid_2d_type), intent(inout) :: self

  call self%clear()

end subroutine ugrid_2d_destructor

end module ugrid_2d_mod
