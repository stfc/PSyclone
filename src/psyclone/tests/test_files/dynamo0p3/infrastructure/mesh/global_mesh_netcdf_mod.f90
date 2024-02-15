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

!> @brief  Describes the cell ordering within the global mesh

!> @details This object holds the connectivities that fully
!>          describe the 2D topology of the global mesh

module global_mesh_netcdf_mod

  use constants_mod,                  only: r_def, i_def, str_max_filename, &
                                            str_def, degrees_to_radians, l_def
  use linked_list_data_mod,           only: linked_list_data_type
  use log_mod,                        only: log_event, log_scratch_space, &
                                            LOG_LEVEL_ERROR, LOG_LEVEL_TRACE
  use global_mesh_base_mod,  only: global_mesh_base_type                                        

  implicit none

  private

  type, extends(global_mesh_base_type), public :: global_mesh_netcdf_type

    private


  contains
    
    final :: global_mesh_netcdf_destructor

  end type global_mesh_netcdf_type

  interface global_mesh_netcdf_type
    module procedure global_mesh_netcdf_constructor
    module procedure global_mesh_netcdf_constructor_unit_test_data
  end interface

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
  function global_mesh_netcdf_constructor( filename, global_mesh_name ) result(self)

    use ugrid_2d_mod,   only: ugrid_2d_type
    use ugrid_file_mod, only: ugrid_file_type
    use ncdf_quad_mod,  only: ncdf_quad_type

    implicit none

    character(*), intent(in) :: filename
    character(*), intent(in) :: global_mesh_name

    type(global_mesh_netcdf_type) :: self

    type(ugrid_2d_type) :: ugrid_2d

    class(ugrid_file_type), allocatable :: file_handler

    ! dimensions from file
    integer(i_def) :: nvert_in
    integer(i_def) :: nface_in
    integer(i_def) :: nedge_in
    integer(i_def) :: num_nodes_per_face
    integer(i_def) :: num_nodes_per_edge
    integer(i_def) :: num_edges_per_face
    integer(i_def) :: max_num_faces_per_node
    integer(i_def) :: ntarget_meshes

  ! Periodic in E-W direction
    logical(l_def)     :: periodic_x
  ! Periodic in N-S direction
    logical(l_def)     :: periodic_y

  ! Type of mesh that the global mesh describes
    character(str_def) :: mesh_class

  ! Horizontal coords of vertices in full domain
    real(r_def), allocatable :: vert_coords(:,:)
  ! Horizontal coords of cells in full domain
    real(r_def), allocatable :: cell_coords(:,:)
  ! Full domain cell to cell connectivities
    integer(i_def), allocatable :: cell_next_2d(:,:)
  ! Full domain vertices on a cell
    integer(i_def), allocatable :: vert_on_cell_2d(:,:)
  ! Full domain edges on a cell
    integer(i_def), allocatable :: edge_on_cell_2d(:,:)
    character(str_def), allocatable :: target_global_mesh_names(:)

    allocate( ncdf_quad_type :: file_handler )
    call ugrid_2d%set_file_handler( file_handler )
    call ugrid_2d%set_from_file_read( trim(global_mesh_name), trim(filename) )
    call ugrid_2d%get_dimensions                           &
            ( num_nodes              = nvert_in,           &
              num_edges              = nedge_in,           &
              num_faces              = nface_in,           &
              num_nodes_per_face     = num_nodes_per_face, &
              num_edges_per_face     = num_edges_per_face, &
              num_nodes_per_edge     = num_nodes_per_edge, &
              max_num_faces_per_node = max_num_faces_per_node )


    call ugrid_2d%get_metadata                          &
            ( mesh_class        = mesh_class,      &
              periodic_x        = periodic_x,      &
              periodic_y        = periodic_y,      &
              nmaps             = ntarget_meshes,  &
              target_mesh_names = target_global_mesh_names )


    allocate( vert_coords(2, nvert_in) )
    call ugrid_2d%get_node_coords(vert_coords)

    allocate( cell_coords(2, nface_in) )
    cell_coords = ugrid_2d%get_face_coords()

    allocate( cell_next_2d( num_edges_per_face, nface_in ) )
    call ugrid_2d%get_face_face_connectivity( cell_next_2d )

    allocate( vert_on_cell_2d( num_nodes_per_face, nface_in ) )
    call ugrid_2d%get_face_node_connectivity( vert_on_cell_2d )

    allocate( edge_on_cell_2d( num_edges_per_face, nface_in ) )
    call ugrid_2d%get_face_edge_connectivity( edge_on_cell_2d )

    self%global_mesh_base_type = &
      global_mesh_base_type(global_mesh_name, nvert_in, nedge_in, nface_in, &
                            max_num_faces_per_node, num_nodes_per_face,     &
                            num_nodes_per_edge,                             &
                            num_edges_per_face, mesh_class, periodic_x,     &
                            periodic_y, ntarget_meshes,                     &
                            target_global_mesh_names,           &
                            vert_coords, cell_coords, cell_next_2d,         &
                            vert_on_cell_2d,      &
                            edge_on_cell_2d)
    deallocate(vert_coords)
    deallocate(cell_coords)
    deallocate(cell_next_2d)
    deallocate(vert_on_cell_2d)
    deallocate(edge_on_cell_2d)

  end function global_mesh_netcdf_constructor

  !===========================================================================
  !> @brief Constructs a small example mesh for unit testing purposes.
  !>
  !> @return 2D global mesh object based on a 9-cell global mesh. 3x3 cell
  !>         arrangement of quadrilateral cells.
  !>
  function global_mesh_netcdf_constructor_unit_test_data() result (self)

    implicit none

    type(global_mesh_netcdf_type) :: self

    self%global_mesh_base_type = global_mesh_base_type()

  end function global_mesh_netcdf_constructor_unit_test_data

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Destroys a global_mesh object when it is finished with.
  !>
  subroutine global_mesh_netcdf_destructor(self)

    implicit none

    type (global_mesh_netcdf_type), intent(inout) :: self

    !> @todo Is there a reason why this is a separate function? It seems
    !>       like a global mesh should be immutable.
    call self%global_mesh_base_type%clear()

  end subroutine global_mesh_netcdf_destructor

end module global_mesh_netcdf_mod
