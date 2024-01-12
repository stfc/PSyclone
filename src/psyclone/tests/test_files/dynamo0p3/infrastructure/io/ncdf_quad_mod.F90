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

!>  @brief   File handler for NetCDF ugrid files.
!>  @details Implementation of the ugrid file class for quads in NetCDF format.
!-------------------------------------------------------------------------------
module ncdf_quad_mod

use constants_mod,  only : r_def, i_def, l_def, str_def, str_long,             &
                           str_max_filename, r_ncdf, i_native
use ugrid_file_mod, only : ugrid_file_type
use netcdf,         only : nf90_max_name, nf90_open, nf90_write, nf90_noerr,   &
                           nf90_strerror, nf90_put_var, nf90_get_var,          &
                           nf90_get_att, nf90_inquire, nf90_inquire_variable,  &
                           nf90_def_var, nf90_inq_varid, nf90_int, nf90_double,&
                           nf90_clobber, nf90_enddef, nf90_inquire_dimension,  &
                           nf90_inq_dimid, nf90_def_dim, nf90_create,          &
                           nf90_inq_attname, nf90_inquire_attribute,           &
                           nf90_redef, nf90_close, nf90_put_att,               &
                           nf90_64bit_offset
use log_mod,        only : log_event, log_scratch_space, LOG_LEVEL_ERROR,      &
                           LOG_LEVEL_INFO

implicit none

private

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------

integer(i_def), parameter :: TWO  = 2                  !< Two
integer(i_def), parameter :: FOUR = 4                  !< Four

! Ranks for each variable.
integer(i_def), parameter :: MESH_RANK            = 0
integer(i_def), parameter :: MESH_FACE_NODES_RANK = 2  !< Rank of face-node connectivity arrays
integer(i_def), parameter :: MESH_EDGE_NODES_RANK = 2  !< Rank of edge-node connectivity arrays
integer(i_def), parameter :: MESH_FACE_EDGES_RANK = 2  !< Rank of face-edge connectivity arrays
integer(i_def), parameter :: MESH_FACE_LINKS_RANK = 2  !< Rank of face-face connectivity arrays
integer(i_def), parameter :: MESH_MESH_LINKS_RANK = 2  !< Rank of mesh-mesh connectivity arrays
integer(i_def), parameter :: MESH_NODE_X_RANK     = 1  !< Rank of node x coordinate array
integer(i_def), parameter :: MESH_NODE_Y_RANK     = 1  !< Rank of node y coordinate array
integer(i_def), parameter :: MESH_FACE_X_RANK     = 1  !< Rank of face x coordinate array
integer(i_def), parameter :: MESH_FACE_Y_RANK     = 1  !< Rank of face y coordinate array

! Note: For spherical coordinates x is equivalent to longitude
!                                 y is equivalent to latitude

!-------------------------------------------------------------------------------
!> @brief   NetCDF quad file type
!> @details Implements the ugrid file type for NetCDF files storing 2D quads.
!-------------------------------------------------------------------------------

type, extends(ugrid_file_type), public :: ncdf_quad_type

  private

  integer(i_def)              :: ncid       !< NetCDF file ID
  character(str_max_filename) :: file_name  !< Filename
  character(nf90_max_name)    :: mesh_name
  character(str_def)          :: mesh_class !< Primitive class of mesh,
                                            !< i.e. sphere, plane
  logical(l_def)              :: periodic_x !< Periodic in E-W direction
  logical(l_def)              :: periodic_y !< Periodic in N-S direction

  character(str_long) :: constructor_inputs !< Inputs to ugrid_generator for this mesh

  character(str_def)  :: coord_units_x
  character(str_def)  :: coord_units_y

  character(nf90_max_name), allocatable :: target_mesh_names(:)

  ! Dimension values
  integer(i_def) :: nmesh_nodes          !< Number of nodes
  integer(i_def) :: nmesh_edges          !< Number of edges
  integer(i_def) :: nmesh_faces          !< Number of faces
  integer(i_def) :: nmesh_targets        !< Number of mesh(es) to map to


  ! Dimension ids
  integer(i_def) :: nmesh_nodes_dim_id   !< NetCDF-assigned ID for number of nodes
  integer(i_def) :: nmesh_edges_dim_id   !< NetCDF-assigned ID for number of edges
  integer(i_def) :: nmesh_faces_dim_id   !< NetCDF-assigned ID for number of faces

  integer(i_def), allocatable :: ntargets_per_source_dim_id(:)
                                         !< NetCDF-assigned ID for number of mesh targets
  integer(i_def) :: two_dim_id           !< NetCDF-assigned ID for constant two
  integer(i_def) :: four_dim_id          !< NetCDF-assigned ID for constant four

  ! Variable ids
  integer(i_def) :: mesh_id              !< NetCDF-assigned ID this mesh
  integer(i_def) :: mesh_edge_nodes_id   !< NetCDF-assigned ID for the edge-node connectivity
  integer(i_def) :: mesh_face_nodes_id   !< NetCDF-assigned ID for the face-node connectivity
  integer(i_def) :: mesh_face_edges_id   !< NetCDF-assigned ID for the face-edge connectivity
  integer(i_def) :: mesh_face_links_id   !< NetCDF-assigned ID for the face-face connectivity
  integer(i_def) :: mesh_node_x_id       !< NetCDF-assigned ID for node x-coordinates
  integer(i_def) :: mesh_node_y_id       !< NetCDF-assigned ID for node y-coordinates
  integer(i_def) :: mesh_face_x_id       !< NetCDF-assigned ID for cell x-coordinates
  integer(i_def) :: mesh_face_y_id       !< NetCDF-assigned ID for cell y-coordinates

  integer(i_def), allocatable :: mesh_mesh_links_id(:)
                                         !< NetCDF-assigned ID for the mesh-mesh connectivity

contains

  procedure :: read_mesh
  procedure :: read_map
  procedure :: get_dimensions
  procedure :: get_mesh_names
  procedure :: get_n_meshes
  procedure :: is_mesh_present
  procedure :: file_open
  procedure :: file_close
  procedure :: file_new

end type ncdf_quad_type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

!-------------------------------------------------------------------------------
!>  @brief Open an existing NetCDF file.
!>
!>  @param[in,out]  self      The NetCDF file object.
!>  @param[in]      file_name Name of the file to open.
!-------------------------------------------------------------------------------

subroutine file_open(self, file_name)
  implicit none

  ! Arguments
  class(ncdf_quad_type), intent(inout) :: self
  character(len=*),      intent(in)    :: file_name

  ! Internal variables
  integer(i_def) :: ierr
  character(*), parameter :: routine = 'file_open'
  character(str_long) :: cmess

  self%file_name = file_name

  cmess = 'Opening file, "'//trim(self%file_name)//'"'
  ierr = nf90_open( trim(self%file_name), nf90_write, self%ncid )
  call check_err(ierr, routine, cmess)

  return
end subroutine file_open


!-------------------------------------------------------------------------------
!>  @brief Closes a NetCDF file.
!>
!>  @param[in]  self   The NetCDF file object.
!-------------------------------------------------------------------------------

subroutine file_close(self)
  implicit none

  ! Arguments
  class(ncdf_quad_type), intent(inout) :: self

  ! Internal variables
  integer(i_def) :: ierr
  character(*), parameter :: routine = 'file_close'
  character(str_long) :: cmess

  cmess = 'Closing file, "'//trim(self%file_name)//'"'
  ierr = nf90_close( self%ncid )
  call check_err(ierr, routine, cmess)

  return
end subroutine file_close

!-------------------------------------------------------------------------------
!>  @brief       Create a new NetCDF file.
!>  @description Creates an opens a new, clean NetCDF file. If a file of the
!>               same name already exists, this routine will clobber it.
!>
!>  @param[in,out]  self      The NetCDF file object.
!>  @param[in]      file_name The name of the file to create/open.
!-------------------------------------------------------------------------------

subroutine file_new(self, file_name)
  implicit none

  ! Arguments
  class(ncdf_quad_type), intent(inout) :: self
  character(len=*),      intent(in)    :: file_name

  ! Internal variables
  integer(i_def) :: ierr
  character(*), parameter :: routine = 'file_new'
  character(str_long) :: cmess

  self%file_name = file_name

  ! Create the NetCDF file with 64-bit offsets to support large file sizes
  cmess = 'Creating file, "'//trim(self%file_name)//'"'
  ierr = nf90_create( path=trim(self%file_name),                 &
                      cmode=ior(nf90_clobber,nf90_64bit_offset), &
                      ncid=self%ncid )
  call check_err(ierr, routine, cmess)

  return
end subroutine file_new



!-------------------------------------------------------------------------------
!>  @brief   Defines NetCDF variables in the netCDF file.
!>  @details Tells NetCDF what variables are going to be in the file.
!>           Array lengths are specified via the pre-existing NetCDF dimension
!>           IDs, which were obtained elsewhere in this module.
!>
!>  @param[in,out]  self   The NetCDF file object.
!-------------------------------------------------------------------------------

subroutine define_variables(self)

  implicit none

  ! Arguments
  class(ncdf_quad_type), intent(inout) :: self

  ! Internal variables
  integer(i_def) :: ierr, i
  integer(i_def) :: zero_sized(0)

  ! Variable shapes
  integer(i_def) :: mesh_face_nodes_dims(MESH_FACE_NODES_RANK)
  integer(i_def) :: mesh_edge_nodes_dims(MESH_EDGE_NODES_RANK)
  integer(i_def) :: mesh_face_edges_dims(MESH_FACE_EDGES_RANK)
  integer(i_def) :: mesh_face_links_dims(MESH_FACE_LINKS_RANK)
  integer(i_def) :: mesh_mesh_links_dims(MESH_MESH_LINKS_RANK)
  integer(i_def) :: mesh_node_x_dims(MESH_NODE_X_RANK)
  integer(i_def) :: mesh_node_y_dims(MESH_NODE_Y_RANK)
  integer(i_def) :: mesh_face_x_dims(MESH_FACE_X_RANK)
  integer(i_def) :: mesh_face_y_dims(MESH_FACE_Y_RANK)

  character(*), parameter :: routine = 'define_variables'
  character(str_long) :: cmess
  character(str_long) :: var_name


  cmess = 'Defining '//trim(self%mesh_name)
  ierr = nf90_def_var( self%ncid, trim(self%mesh_name), &
                      nf90_int, zero_sized, self%mesh_id )
  call check_err(ierr, routine, cmess)


  mesh_face_nodes_dims(1) = self%four_dim_id
  mesh_face_nodes_dims(2) = self%nmesh_faces_dim_id
  var_name = trim(self%mesh_name)//'_face_nodes'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),       &
                       nf90_int, mesh_face_nodes_dims,  &
                       self%mesh_face_nodes_id )
  call check_err(ierr, routine, cmess)

  mesh_edge_nodes_dims(1) = self%two_dim_id
  mesh_edge_nodes_dims(2) =self%nmesh_edges_dim_id
  var_name = trim(self%mesh_name)//'_edge_nodes'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),       &
                       nf90_int, mesh_edge_nodes_dims,  &
                       self%mesh_edge_nodes_id )
  call check_err(ierr, routine, cmess)

  mesh_face_edges_dims(1) = self%four_dim_id
  mesh_face_edges_dims(2) = self%nmesh_faces_dim_id
  var_name = trim(self%mesh_name)//'_face_edges'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),       &
                       nf90_int, mesh_face_edges_dims,  &
                       self%mesh_face_edges_id )
  call check_err(ierr, routine, cmess)

  mesh_face_links_dims(1) = self%four_dim_id
  mesh_face_links_dims(2) = self%nmesh_faces_dim_id
  var_name = trim(self%mesh_name)//'_face_links'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),       &
                       nf90_int, mesh_face_links_dims,  &
                       self%mesh_face_links_id )
  call check_err(ierr, routine, cmess)

  mesh_node_x_dims(1) = self%nmesh_nodes_dim_id
  var_name = trim(self%mesh_name)//'_node_x'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),       &
                       nf90_double, mesh_node_x_dims,   &
                       self%mesh_node_x_id )
  call check_err(ierr, routine, cmess)

  mesh_node_y_dims(1) = self%nmesh_nodes_dim_id
  var_name = trim(self%mesh_name)//'_node_y'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),       &
                       nf90_double, mesh_node_y_dims,   &
                       self%mesh_node_y_id )
  call check_err(ierr, routine, cmess)


  mesh_face_x_dims(1) = self%nmesh_faces_dim_id
  var_name = trim(self%mesh_name)//'_face_x'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),       &
                       nf90_double, mesh_face_x_dims,   &
                       self%mesh_face_x_id )
  call check_err(ierr, routine, cmess)

  mesh_face_y_dims(1) = self%nmesh_faces_dim_id
  var_name = trim(self%mesh_name)//'_face_y'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),       &
                       nf90_double, mesh_face_y_dims,   &
                       self%mesh_face_y_id )
  call check_err(ierr, routine, cmess)

  ! Define variables which this mesh maps to
  do i=1, self%nmesh_targets

    var_name=trim(self%mesh_name)//'_'//trim(self%target_mesh_names(i))//'_map'
    cmess = 'Defining '//trim(var_name)

    mesh_mesh_links_dims(1) = self%ntargets_per_source_dim_id(i)
    mesh_mesh_links_dims(2) = self%nmesh_faces_dim_id

    ierr = nf90_def_var( self%ncid, trim(var_name),      &
                         nf90_int, mesh_mesh_links_dims, &
                         self%mesh_mesh_links_id(i) )

    call check_err(ierr,routine, cmess)
  end do

  return
end subroutine define_variables

!-------------------------------------------------------------------------------
!>  @brief   Assigns attributes to the NetCDF variables.
!>  @details Adds additional information to NetCDF variables that should have
!>           already been defined elsewhere in this module.  Attributes include
!>           variable names and descriptions.
!>
!>  @param[in]   self   The NetCDF file object.
!-------------------------------------------------------------------------------

subroutine assign_attributes(self)
  implicit none

  ! Arguments
  class(ncdf_quad_type), intent(in) :: self

  ! Internal variables
  integer(i_def) :: ierr, id, i

  character(str_def)  :: std_x_name
  character(str_def)  :: std_y_name
  character(str_def)  :: long_x_name
  character(str_def)  :: long_y_name
  character(str_def)  :: coord_units_x
  character(str_def)  :: coord_units_y

  character(*), parameter :: routine = 'assign_attributes'
  character(str_long) :: cmess

  character(str_long) :: target_mesh_names_str
  character(str_long) :: attname
  character(str_def)  :: lchar_px
  character(str_def)  :: lchar_py

  character(nf90_max_name) :: var_name
  character(nf90_max_name) :: source_mesh_name
  character(nf90_max_name) :: target_mesh_name

  target_mesh_names_str = ''

  !===================================================================
  ! 1.0 Add attributes for mesh topology variable
  !===================================================================
  id = self%mesh_id
  ierr = nf90_inquire_variable( ncid=self%ncid, varid=id, name=var_name )
  write(cmess,'(A,I0)') 'Inquiring name of variable, id:',id
  call check_err(ierr, routine, cmess)

  !===================================================================
  attname = 'cf_role'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), 'mesh_topology')
  call check_err(ierr, routine, cmess)

  attname = 'mesh_class'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       trim(self%mesh_class))
  call check_err(ierr, routine, cmess)

  attname = 'periodic_x'
  write(lchar_px, '(L1)') self%periodic_x
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       trim(adjustl(lchar_px)))
  call check_err(ierr, routine, cmess)

  attname = 'periodic_y'
  write(lchar_py, '(L1)') self%periodic_y
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       trim(adjustl(lchar_py)))
  call check_err(ierr, routine, cmess)

  attname = 'constructor_inputs'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       trim(self%constructor_inputs) )
  call check_err(ierr, routine, cmess)

  attname = 'n_mesh_maps'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       size(self%target_mesh_names) )
  call check_err(ierr, routine, cmess)

  if (allocated(self%target_mesh_names)) then
    attname = 'maps_to'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(var_name)//'"'
    do i=1, size(self%target_mesh_names)
      write(target_mesh_names_str,'(A)')               &
           trim(adjustl(target_mesh_names_str))//' '// &
           trim(adjustl(self%target_mesh_names(i)))
    end do
    ierr = nf90_put_att( self%ncid, id, trim(attname), &
                         trim(adjustl(target_mesh_names_str)) )
    call check_err(ierr, routine, cmess)
  end if

  attname = 'long_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       'Topology data of 2D unstructured mesh')
  call check_err(ierr, routine, cmess)

  attname = 'topology_dimension'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), [2])
  call check_err(ierr, routine, cmess)

  attname = 'node_coordinates'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname),      &
                       trim(self%mesh_name)//'_node_x '// &
                       trim(self%mesh_name)//'_node_y' )
  call check_err(ierr, routine, cmess)

  attname = 'face_coordinates'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname),      &
                       trim(self%mesh_name)//'_face_x '// &
                       trim(self%mesh_name)//'_face_y' )
  call check_err(ierr, routine, cmess)

  attname = 'face_node_connectivity'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       trim(self%mesh_name)//'_face_nodes')
  call check_err(ierr, routine, cmess)

  attname = 'edge_node_connectivity'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       trim(self%mesh_name)//'_edge_nodes' )
  call check_err(ierr, routine, cmess)

  attname = 'face_edge_connectivity'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       trim(self%mesh_name)//'_face_edges' )
  call check_err(ierr, routine, cmess)

  attname = 'face_face_connectivity'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       trim(self%mesh_name)//'_face_links' )
  call check_err(ierr, routine, cmess)


  !===================================================================
  ! 2.0 Add attributes for mesh face nodes variable
  !===================================================================
  id = self%mesh_face_nodes_id
  ierr = nf90_inquire_variable( ncid=self%ncid, varid=id, name=var_name )
  write(cmess,'(A,I0)') 'Inquiring name of variable, id:',id
  call check_err(ierr, routine, cmess)
  !===================================================================

  attname = 'cf_role'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr    = nf90_put_att( self%ncid, id, trim(attname), &
                          'face_node_connectivity' )
  call check_err(ierr, routine, cmess)

  attname = 'long_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       'Maps every quadrilateral face to its four corner nodes.')
  call check_err(ierr, routine, cmess)

  attname = 'start_index'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), [1])
  call check_err(ierr, routine, cmess)


  !===================================================================
  ! 3.0 Add attributes for mesh edge nodes variable
  !===================================================================
  id = self%mesh_edge_nodes_id
  ierr = nf90_inquire_variable( ncid=self%ncid, varid=id, name=var_name )
  write(cmess,'(A,I0)') 'Inquiring name of variable, id:',id
  call check_err(ierr, routine, cmess)
  !===================================================================
  attname = 'cf_role'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       'edge_node_connectivity' )
  call check_err(ierr, routine, cmess)

  attname = 'long_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       'Maps every edge to the two nodes that it connects.' )
  call check_err(ierr, routine, cmess)

  attname = 'start_index'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), [1] )
  call check_err(ierr, routine, cmess)


  !===================================================================
  ! 4.0 Add attributes for mesh face edges variable
  !===================================================================
  id = self%mesh_face_edges_id
  ierr = nf90_inquire_variable( ncid=self%ncid, varid=id, name=var_name )
  write(cmess,'(A,I0)') 'Inquiring name of variable, id:',id
  call check_err(ierr, routine, cmess)
  !===================================================================
  attname = 'cf_role'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       'face_edge_connectivity' )
  call check_err(ierr, routine, cmess)

  attname = 'long_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       'Maps every quadrilateral face to its four edges.' )
  call check_err(ierr, routine, cmess)

  attname = 'start_index'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), [1] )
  call check_err(ierr, routine, cmess)


  !===================================================================
  ! 5.0 Add attributes for mesh face links variable
  !===================================================================
  id   = self%mesh_face_links_id
  ierr = nf90_inquire_variable( ncid=self%ncid, varid=id, name=var_name )
  write(cmess,'(A,I0)') 'Inquiring name of variable, id:',id
  call check_err(ierr, routine, cmess)
  !===================================================================
  attname = 'cf_role'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       'face_face_connectivity' )
  call check_err(ierr, routine, cmess)

  attname = 'long_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       'Indicates which other faces neighbour each face.' )
  call check_err(ierr, routine, cmess)

  attname = 'start_index'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), [1] )
  call check_err(ierr, routine, cmess)

  attname = 'flag_values'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), [-1] )
  call check_err(ierr, routine, cmess)

  attname = 'flag_meanings'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), 'out_of_mesh' )
  call check_err(ierr, routine, cmess)


  !===================================================================
  ! 6.0 Add attributes for mesh node coordinate variables
  !===================================================================
  select case (trim(self%mesh_class))
  case ('sphere')
    std_x_name  = 'longitude'
    std_y_name  = 'latitude'
    long_x_name = 'longitude of 2D mesh nodes.'
    long_y_name = 'latitude of 2D mesh nodes.'

  case ('plane')
    std_x_name  = 'projection_x_coordinate'
    std_y_name  = 'projection_y_coordinate'
    long_x_name = 'x coordinate of 2D mesh nodes.'
    long_y_name = 'y coordinate of 2D mesh nodes.'
  end select

  coord_units_x = self%coord_units_x
  coord_units_y = self%coord_units_y

  id   = self%mesh_node_x_id
  ierr = nf90_inquire_variable( ncid=self%ncid, varid=id, name=var_name )
  write(cmess,'(A,I0)') 'Inquiring name of variable, id:',id
  call check_err(ierr, routine, cmess)
  !===================================================================
  attname = 'standard_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), std_x_name )
  call check_err(ierr, routine, cmess)

  attname = 'long_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), long_x_name )
  call check_err(ierr, routine, cmess)

  attname = 'units'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), coord_units_x )
  call check_err(ierr, routine, cmess)



  id = self%mesh_node_y_id
  ierr = nf90_inquire_variable( ncid=self%ncid, varid=id, name=var_name )
  write(cmess,'(A,I0)') 'Inquiring name of variable, id:',id
  call check_err(ierr, routine, cmess)
  !===================================================================
  attname = 'standard_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), std_y_name )
  call check_err(ierr, routine, cmess)

  attname = 'long_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), long_y_name )
  call check_err(ierr, routine, cmess)

  attname = 'units'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), coord_units_y )
  call check_err(ierr, routine, cmess)


  !===================================================================
  ! 7.0 Add attributes for mesh face coordinate variables
  !===================================================================
  select case (trim(self%mesh_class))
  case ('sphere')
    std_x_name  = 'longitude'
    std_y_name  = 'latitude'
    long_x_name = 'longitude of 2D face centres'
    long_y_name = 'latitude of 2D face centres'
  case ('plane')
    std_x_name  = 'projection_x_coordinate'
    std_y_name  = 'projection_y_coordinate'
    long_x_name = 'x coordinate of 2D face centres'
    long_y_name = 'y coordinate of 2D face centres'
  end select

  id   = self%mesh_face_x_id
  ierr = nf90_inquire_variable( ncid=self%ncid, varid=id, name=var_name )
  write(cmess,'(A,I0)') 'Inquiring name of variable, id:',id
  call check_err(ierr, routine, cmess)
  !===================================================================
  attname = 'standard_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), std_x_name )
  call check_err(ierr, routine, cmess)

  attname = 'long_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), long_x_name )
  call check_err(ierr, routine, cmess)

  attname = 'units'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), coord_units_x )
  call check_err(ierr, routine, cmess)



  id = self%mesh_face_y_id
  ierr = nf90_inquire_variable( ncid=self%ncid, varid=id, name=var_name )
  write(cmess,'(A,I0)') 'Inquiring name of variable, id:',id
  call check_err(ierr, routine, cmess)
  !===================================================================
  attname = 'standard_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), std_y_name )
  call check_err(ierr, routine, cmess)

  attname = 'long_name'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), long_y_name )
  call check_err(ierr, routine, cmess)

  attname = 'units'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), coord_units_y )
  call check_err(ierr, routine, cmess)


  !===================================================================
  ! 8.0 Add attributes for mesh mesh link variables
  !===================================================================
  if (allocated(self%mesh_mesh_links_id)) then

    do i=1, size(self%mesh_mesh_links_id)

      id      = self%mesh_mesh_links_id(i)
      ierr = nf90_inquire_variable( ncid=self%ncid, varid=id, name=var_name )
      write(cmess,'(A,I0)') 'Inquiring name of variable, id:',id
      call check_err(ierr, routine, cmess)
      source_mesh_name = var_name(1:index(var_name,'_')-1)
      target_mesh_name = var_name(index(var_name,'_')+1:index(var_name,'_',back=.true.)-1)


      attname = 'cf_role'
      cmess   = 'Adding attribute "'//trim(attname)// &
                '" to variable "'//trim(var_name)//'"'
      ierr = nf90_put_att( self%ncid, id, trim(attname), &
                           'mesh_mesh_connectivity' )
      call check_err(ierr, routine, cmess)

      attname = 'long_name'
      cmess   = 'Adding attribute "'//trim(attname)// &
                '" to variable "'//trim(var_name)//'"'
      ierr = nf90_put_att( self%ncid, id, trim(attname),                  &
                           'Maps source mesh('//trim(source_mesh_name)//  &
                           ') cell to set of cells on target mesh('//     &
                           trim(target_mesh_name)//').' )
      call check_err(ierr, routine, cmess)

      attname = 'start_index'
      cmess   = 'Adding attribute "'//trim(attname)// &
                '" to variable "'//trim(var_name)//'"'
      ierr = nf90_put_att( self%ncid, id, trim(attname), [1] )
      call check_err(ierr, routine, cmess)

    end do

  end if

  return
end subroutine assign_attributes

!-------------------------------------------------------------------------------
!>  @brief   Gets dimension ids and variable ids from the open NetCDF file.
!>  @details NetCDF files refer to dimensions and variables by an id, the value
!>           of which is determined by the NetCDF library. This routine finds
!>           dimension and variable ids for all variables of interest in the
!>           open NetCDF file.
!>
!>  @param[in,out] self      The NetCDF file object.
!>  @param[in]     mesh_name Name of mesh topology to get ids for
!-------------------------------------------------------------------------------

subroutine inquire_ids(self, mesh_name)

  implicit none

  ! Arguments
  type(ncdf_quad_type), intent(inout) :: self

  character(str_def), intent(in) :: mesh_name

  ! Internal variables
  integer(i_def) :: ierr

  character(nf90_max_name) :: dim_name
  character(nf90_max_name) :: var_name

  logical(l_def) :: mesh_present

  character(*), parameter :: routine = 'inquire_ids'
  character(str_long) :: cmess

  mesh_present = self%is_mesh_present(mesh_name)

  if (.not. mesh_present) then
    write(log_scratch_space,'(A)') &
         'Mesh '//trim(mesh_name)//' not present in file'
    call log_event(trim(log_scratch_space), LOG_LEVEL_ERROR)
  end if

  cmess = 'Getting mesh netcdf id for"'//trim(mesh_name)//'"'
  ierr = nf90_inq_varid( self%ncid, trim(mesh_name), self%mesh_id )
  call check_err(ierr, routine, cmess)

  ! Numbers of entities
  dim_name = 'n'//trim(mesh_name)//'_node'
  cmess = 'Getting id for '//trim(dim_name)
  ierr = nf90_inq_dimid( self%ncid, trim(dim_name), &
                         self%nmesh_nodes_dim_id )
  call check_err(ierr, routine, cmess)

  dim_name = 'n'//trim(mesh_name)//'_edge'
  cmess = 'Getting id for '//trim(dim_name)
  ierr = nf90_inq_dimid( self%ncid, trim(dim_name), &
                        self%nmesh_edges_dim_id )
  call check_err(ierr, routine, cmess)

  dim_name = 'n'//trim(mesh_name)//'_face'
  cmess = 'Getting id for '//trim(dim_name)
  ierr = nf90_inq_dimid( self%ncid, trim(dim_name), &
                         self%nmesh_faces_dim_id )
  call check_err(ierr, routine, cmess)


  ! Node coordinates
  var_name = trim(mesh_name)//'_node_x'
  cmess = 'Getting id for '//trim(var_name)
  ierr = nf90_inq_varid( self%ncid, trim(var_name), &
                         self%mesh_node_x_id )
  call check_err(ierr, routine, cmess)

  var_name = trim(mesh_name)//'_node_y'
  cmess = 'Getting id for '//trim(var_name)
  ierr = nf90_inq_varid( self%ncid, trim(var_name), &
                         self%mesh_node_y_id )
  call check_err(ierr, routine, cmess)

  ! Face coordinates
  var_name = trim(mesh_name)//'_face_x'
  cmess = 'Getting id for '//trim(var_name)
  ierr = nf90_inq_varid( self%ncid, trim(var_name), &
                         self%mesh_face_x_id )
  call check_err(ierr, routine, cmess)

  var_name = trim(mesh_name)//'_face_y'
  cmess = 'Getting id for '//trim(var_name)
  ierr = nf90_inq_varid( self%ncid, trim(var_name), &
                         self%mesh_face_y_id )
  call check_err(ierr, routine, cmess)

  ! Face node connectivity
  var_name = trim(mesh_name)//'_face_nodes'
  cmess = 'Getting id for '//trim(var_name)
  ierr = nf90_inq_varid( self%ncid, trim(var_name), &
                         self%mesh_face_nodes_id )
  call check_err(ierr, routine, cmess)

  ! Edge node connectivity
  var_name = trim(mesh_name)//'_edge_nodes'
  cmess = 'Getting id for '//trim(var_name)
  ierr = nf90_inq_varid( self%ncid, trim(var_name), &
                         self%mesh_edge_nodes_id )
  call check_err(ierr, routine, cmess)

  ! Face edge connectivity
  var_name = trim(mesh_name)//'_face_edges'
  cmess = 'Getting id for '//trim(var_name)
  ierr = nf90_inq_varid( self%ncid, trim(var_name), &
                         self%mesh_face_edges_id )
  call check_err(ierr, routine, cmess)

  ! Face face connectivity
  var_name = trim(mesh_name)//'_face_links'
  cmess = 'Getting id for '//trim(var_name)
  ierr = nf90_inq_varid( self%ncid, trim(var_name), &
                         self%mesh_face_links_id )
  call check_err(ierr, routine, cmess)


  dim_name = 'Two'
  cmess = 'Getting id for '//trim(dim_name)
  ierr = nf90_inq_dimid(self%ncid, trim(dim_name),  self%two_dim_id)
  call check_err(ierr,routine, cmess)

  dim_name = 'Four'
  cmess = 'Getting id for '//trim(dim_name)
  ierr = nf90_inq_dimid(self%ncid, trim(dim_name), self%four_dim_id)
  call check_err(ierr, routine, cmess)

  return
end subroutine inquire_ids

!-------------------------------------------------------------------------------
!>  @brief   Calls logger on error.
!>  @details Checks the error code returned by the NetCDF file. If an error is
!>           detected, the relevant error message is passed to the logger.
!>
!>  @param[in] ierr    The error code to check.
!>  @param[in] routine The routine name that call the error check
!>  @param[in] cmess   Comment message for the error report
!-------------------------------------------------------------------------------

subroutine check_err(ierr, routine, cmess)
  implicit none

  ! Arguments
  integer(i_def),      intent(in) :: ierr
  character(*),        intent(in) :: routine
  character(str_long), intent(in) :: cmess


  if (ierr /= NF90_NOERR) then
    write(log_scratch_space,*) 'Error in ncdf_quad ['//routine//']: '//  &
        trim(cmess) // ' ' // trim(nf90_strerror(ierr))
    call log_event( trim(log_scratch_space), LOG_LEVEL_ERROR )
  end if

  return
end subroutine check_err

!-------------------------------------------------------------------------------
!>  @brief   Returns the number of mesh topologies described in this file.
!>  @details Scans the variable attributes for cf_role = mesh_topology as
!>           a flag that the variable name relates to a mesh.
!>
!>  @return n_meshes  Integer, The number of mesh topologies in the NetCDf file
!-------------------------------------------------------------------------------
function get_n_meshes(self) result (n_meshes)

  implicit none

  class(ncdf_quad_type), intent(in) :: self

  integer(i_def) :: n_meshes

  character(str_def), allocatable :: mesh_names(:)
  integer(i_def), parameter :: max_n_topologies = 20


  allocate( mesh_names (max_n_topologies) )
  call scan_for_topologies(self, mesh_names, n_meshes)

  deallocate(mesh_names)

  return
end function get_n_meshes

!-------------------------------------------------------------------------------
!>  @brief Returns the names of mesh topologies described in this file.
!>
!>  @param[out] mesh_names  Character[:], Names of the mesh_topologies
!>                          in the NetCDF file
!-------------------------------------------------------------------------------
subroutine get_mesh_names(self, mesh_names)

  implicit none

  class(ncdf_quad_type), intent(in)  :: self
  character(len=*),      intent(out) :: mesh_names(:)

  integer(i_def) :: n_meshes

  call scan_for_topologies(self, mesh_names, n_meshes)

  return
end subroutine get_mesh_names

!-------------------------------------------------------------------------------
!>  @brief   Gets dimension information from the NetCDF file, as integers.
!>  @details Calls NetCDF inquiry functions to determine array lengths, such as
!>           the number of nodes.
!>
!>  @param[in,out]   self                   The NetCDF file object.
!>  @param[in]       mesh_name              Name of the mesh topology
!>  @param[out]      num_nodes              The number of nodes on the mesh.
!>  @param[out]      num_edges              The number of edges on the mesh.
!>  @param[out]      num_faces              The number of faces on the mesh.
!>  @param[out]      num_nodes_per_face     The number of nodes per face.
!>  @param[out]      num_edges_per_face     The number of edges per face.
!>  @param[out]      num_nodes_per_edge     The number of nodes per edge.
!>  @param[out]      max_num_faces_per_node The maximum number of faces surrounding a node.
!-------------------------------------------------------------------------------

subroutine get_dimensions( self,               &
                           mesh_name,          &
                           num_nodes,          &
                           num_edges,          &
                           num_faces,          &
                           num_nodes_per_face, &
                           num_edges_per_face, &
                           num_nodes_per_edge, &
                           max_num_faces_per_node )
  implicit none

  ! Arguments
  class(ncdf_quad_type),  intent(inout) :: self

  character(str_def), intent(in)  :: mesh_name
  integer(i_def),     intent(out) :: num_nodes
  integer(i_def),     intent(out) :: num_edges
  integer(i_def),     intent(out) :: num_faces
  integer(i_def),     intent(out) :: num_nodes_per_face
  integer(i_def),     intent(out) :: num_edges_per_face
  integer(i_def),     intent(out) :: num_nodes_per_edge
  integer(i_def),     intent(out) :: max_num_faces_per_node


  integer(i_def) :: ierr

  character(*), parameter :: routine = 'get_dimensions'
  character(str_long) :: cmess

  call inquire_ids(self, mesh_name)

  ! Get dimension lengths
  cmess = 'Getting number of nodes on mesh, "'//trim(mesh_name)//'"'
  ierr = nf90_inquire_dimension( self%ncid,               &
                                 self%nmesh_nodes_dim_id, &
                                 len=self%nmesh_nodes )
  call check_err(ierr, routine, cmess)

  cmess = 'Getting number of edges on mesh, "'//trim(mesh_name)//'"'
  ierr = nf90_inquire_dimension( self%ncid,               &
                                 self%nmesh_edges_dim_id, &
                                 len=self%nmesh_edges )
  call check_err(ierr, routine, cmess)

  cmess = 'Getting number of faces on mesh, "'//trim(mesh_name)//'"'
  ierr = nf90_inquire_dimension( self%ncid,               &
                                 self%nmesh_faces_dim_id, &
                                 len=self%nmesh_faces )
  call check_err(ierr, routine, cmess)

  num_nodes = self%nmesh_nodes
  num_edges = self%nmesh_edges
  num_faces = self%nmesh_faces

  num_nodes_per_face = 4
  num_edges_per_face = 4
  num_nodes_per_edge = 2
  max_num_faces_per_node = 4

  return
end subroutine get_dimensions

!-------------------------------------------------------------------------------
!>  @brief   Read data from the NetCDF file.
!>  @details Reads coordinate and connectivity information from the NetCDF file.
!>
!>  @param[in,out]  self                     The NetCDF file object.
!>  @param[in]      mesh_name                Name of the mesh topology
!>  @param[out]     mesh_class               Primitive class of mesh
!>  @param[out]     periodic_x               Periodic in E-W direction.
!>  @param[out]     periodic_y               Periodic in N-S direction.
!>  @param[out]     constructor_inputs       Inputs to the ugrid_generator to
!>                                           generate mesh
!>  @param[out]     node_coordinates         Coordinates of each node.
!>  @param[out]     face_coordinates         Coordinates of each face.
!>  @param[out]     coord_units_x            Units of x coordinates.
!>  @param[out]     coord_units_y            Units of y coordinates.
!>  @param[out]     face_node_connectivity   Nodes adjoining each face.
!>  @param[out]     edge_node_connectivity   Nodes adjoining each edge.
!>  @param[out]     face_edge_connectivity   Edges adjoining each face.
!>  @param[out]     face_face_connectivity   Faces adjoining each face (links).
!>  @param[out]     num_targets              Number of mesh maps from mesh
!>  @param[out]     target_mesh_names        Mesh(es) that this mesh has maps for
!-------------------------------------------------------------------------------

subroutine read_mesh( self, mesh_name, mesh_class,                     &
                      periodic_x, periodic_y, constructor_inputs,      &
                      node_coordinates, face_coordinates,              &
                      coord_units_x, coord_units_y,                    &
                      face_node_connectivity, edge_node_connectivity,  &
                      face_edge_connectivity, face_face_connectivity,  &
                      num_targets, target_mesh_names )
  implicit none

  ! Arguments
  class(ncdf_quad_type),  intent(inout) :: self

  character(str_def),  intent(in)  :: mesh_name
  character(str_def),  intent(out) :: mesh_class
  logical(l_def),      intent(out) :: periodic_x
  logical(l_def),      intent(out) :: periodic_y
  character(str_long), intent(out) :: constructor_inputs
  real(r_def),         intent(out) :: node_coordinates(:,:)
  real(r_def),         intent(out) :: face_coordinates(:,:)
  character(str_def),  intent(out) :: coord_units_x
  character(str_def),  intent(out) :: coord_units_y
  integer(i_def),      intent(out) :: face_node_connectivity(:,:)
  integer(i_def),      intent(out) :: edge_node_connectivity(:,:)
  integer(i_def),      intent(out) :: face_edge_connectivity(:,:)
  integer(i_def),      intent(out) :: face_face_connectivity(:,:)
  integer(i_def),      intent(out) :: num_targets
  character(str_def),  intent(out), allocatable :: target_mesh_names(:)

  ! Internal variables
  integer(i_def) :: ierr, upper_bound, i

  character(*), parameter :: routine = 'read_mesh'
  character(str_long) :: cmess
  character(str_long) :: target_mesh_names_str

  ! We need to ensure that netcdf receives data with the appropriate
  ! precision to create temporary arrays to hold real data
  ! converted from/to default precision
  real(r_ncdf), allocatable :: node_coordinates_ncdf(:,:)
  real(r_ncdf), allocatable :: face_coordinates_ncdf(:,:)

  integer(i_def) :: lower1,upper1,lower2,upper2

  character(str_def) :: lchar_px
  character(str_def) :: lchar_py

  lower1 = lbound(node_coordinates, 1)
  lower2 = lbound(node_coordinates, 2)
  upper1 = ubound(node_coordinates, 1)
  upper2 = ubound(node_coordinates, 2)

  allocate(node_coordinates_ncdf(lower1:upper1,lower2:upper2))

  lower1 = lbound(face_coordinates, 1)
  lower2 = lbound(face_coordinates, 2)
  upper1 = ubound(face_coordinates, 1)
  upper2 = ubound(face_coordinates, 2)

  allocate(face_coordinates_ncdf(lower1:upper1,lower2:upper2))

  call inquire_ids(self, mesh_name)

  ! Mesh class
  cmess = 'Getting attribute, "mesh_class"'
  ierr = nf90_get_att( self%ncid, self%mesh_id, &
                       'mesh_class', mesh_class )
  call check_err(ierr, routine, cmess)

  ! Periodic in E-W direction
  cmess = 'Getting attribute, "periodic_x"'
  ierr = nf90_get_att( self%ncid, self%mesh_id, &
                       'periodic_x', lchar_px )
  call check_err(ierr, routine, cmess)
  read(lchar_px, '(L8)') periodic_x

  ! Periodic in N-S direction
  cmess = 'Getting attribute, "periodic_y"'
  ierr = nf90_get_att( self%ncid, self%mesh_id, &
                       'periodic_y', lchar_py )
  call check_err(ierr, routine, cmess)
  read(lchar_py, '(L8)') periodic_y

  ! Ugrid mesh constructor inputs
  cmess = 'Getting attribute, "constructor_inputs"'
  ierr = nf90_get_att( self%ncid, self%mesh_id, &
                       'constructor_inputs', constructor_inputs )
  call check_err(ierr, routine, cmess)

  ! Number of mesh maps
  cmess = 'Getting attribute, "n_mesh_maps"'
  ierr = nf90_get_att( self%ncid, self%mesh_id, &
                       'n_mesh_maps', num_targets )
  call check_err(ierr, routine, cmess)
  self%nmesh_targets = num_targets

  ! Read in names of the target meshes to map to.
  ! Can't read in the actual maps as they require
  ! the global mesh ids of the source and target.
  ! The target may not have been read in at this point.
  ! So target names are read, and the maps read in after
  ! global meshes have been read in.

  ! integer global mesh ids assigned by the global_mesh_mod/collection
  if (self%nmesh_targets > 0 ) then

    allocate(target_mesh_names(self%nmesh_targets))

    ! 1.0 Find out the target mesh names
    ierr = nf90_inquire_attribute(self%ncid, self%mesh_id, "maps_to")
    if (ierr == nf90_noerr) then

      cmess = 'Getting attribute, "maps_to"'
      ierr = nf90_get_att( self%ncid, self%mesh_id, &
                           'maps_to', target_mesh_names_str )
      call check_err(ierr, routine, cmess)

      do i =1, self%nmesh_targets
        ! Get the a target from the list
        upper_bound=index(target_mesh_names_str,' ')
        target_mesh_names(i) = trim(target_mesh_names_str(1:upper_bound))

        target_mesh_names_str(1:upper_bound) = ' '
        target_mesh_names_str = trim(adjustl(target_mesh_names_str))
      end do
    end if

  end if ! self%nmesh_targets > 0



  ! Coordinate units
  cmess = 'Getting x-coord units for mesh "'//trim(mesh_name)//'"'
  ierr = nf90_get_att( self%ncid, self%mesh_node_x_id, "units", coord_units_x )
  call check_err(ierr, routine, cmess)

  cmess = 'Getting y-coord units for mesh "'//trim(mesh_name)//'"'
  ierr = nf90_get_att( self%ncid, self%mesh_node_y_id, "units", coord_units_y )
  call check_err(ierr, routine, cmess)



  ! Node coordinates
  cmess = 'Getting node x coords for mesh "'//trim(mesh_name)//'"'
  ierr = nf90_get_var( self%ncid, self%mesh_node_x_id, &
                       node_coordinates_ncdf(1,:))
  call check_err(ierr, routine, cmess)

  cmess = 'Getting node y coords for mesh "'//trim(mesh_name)//'"'
  ierr = nf90_get_var( self%ncid, self%mesh_node_y_id, &
                       node_coordinates_ncdf(2,:))
  call check_err(ierr, routine, cmess)



  ! Face coordinates
  cmess = 'Getting face x coords for mesh "'//trim(mesh_name)//'"'
  ierr = nf90_get_var( self%ncid, self%mesh_face_x_id, &
                       face_coordinates_ncdf(1,:))
  call check_err(ierr, routine, cmess)

  cmess = 'Getting face y coords for mesh "'//trim(mesh_name)//'"'
  ierr = nf90_get_var( self%ncid, self%mesh_face_y_id, &
                       face_coordinates_ncdf(2,:))
  call check_err(ierr, routine, cmess)



  ! Face node connectivity
  cmess = 'Getting face-node connectivity for mesh "'//trim(mesh_name)//'"'
  ierr = nf90_get_var( self%ncid, self%mesh_face_nodes_id, &
                       face_node_connectivity(:,:) )
  call check_err(ierr, routine, cmess)



  ! Edge node connectivity
  cmess = 'Getting edge-node connectivity for mesh "'//trim(mesh_name)//'"'
  ierr = nf90_get_var( self%ncid, self%mesh_edge_nodes_id, &
                       edge_node_connectivity(:,:) )
  call check_err(ierr, routine, cmess)



  ! Face edge connectivity
  cmess = 'Getting face-edge connectivity for mesh "'//trim(mesh_name)//'"'
  ierr = nf90_get_var( self%ncid, self%mesh_face_edges_id, &
                       face_edge_connectivity(:,:) )
  call check_err(ierr, routine, cmess)



  ! Face face connectivity
  cmess = 'Getting face-face connectivity for mesh "'//trim(mesh_name)//'"'
  ierr = nf90_get_var( self%ncid, self%mesh_face_links_id, &
                       face_face_connectivity(:,:))
  call check_err(ierr, routine, cmess)

  ! Pass back to r_def arrays and deallocate
  node_coordinates(:,:) = real( node_coordinates_ncdf(:,:), kind=r_def )
  face_coordinates(:,:) = real( face_coordinates_ncdf(:,:), kind=r_def )
  deallocate(node_coordinates_ncdf)
  deallocate(face_coordinates_ncdf)

  return
end subroutine read_mesh

!-------------------------------------------------------------------------------
!>  @brief   Read mesh map data from the NetCDF file.
!>  @details Source and Target mesh names are used to identify
!>           the relevant intergrid map in the mesh input file.
!>
!>  @param[in]      source_mesh_name    Name of the source mesh object
!>  @param[in]      target_mesh_name    Name of the target mesh object
!>  @param[out]     mesh_map            Intergrid mapping array which maps
!>                                      source mesh cells to target mesh
!>                                      cells. Allocatable integer array,
!>                                      returned as
!>                                      [n target cells per source cell,
!>                                       n source cells]
!-------------------------------------------------------------------------------
subroutine read_map( self,             &
                     source_mesh_name, &
                     target_mesh_name, &
                     mesh_map )

  implicit none

  ! Arguments
  class(ncdf_quad_type),  intent(in) :: self

  character(str_def), intent(in)  :: source_mesh_name
  character(str_def), intent(in)  :: target_mesh_name
  integer(i_def),     intent(out), allocatable :: mesh_map(:,:)

  character(*), parameter :: routine = 'read_map'

  integer(i_native) :: mesh_map_id
  integer(i_native) :: source_cells_id
  integer(i_native) :: target_cells_per_source_cell_id
  integer(i_native) :: ierr

  integer(i_def) :: source_ncells
  integer(i_def) :: target_cells_per_source_cell

  character(nf90_max_name) :: var_name, dim_name
  character(str_long)      :: cmess

  ! 1.0 Get number of cells for source map
  !=====================================================
  dim_name = 'n'//trim(source_mesh_name)//'_face'

  cmess = 'Getting '//trim(dim_name)//' id'
  ierr = nf90_inq_dimid( self%ncid, &
                         dim_name,  &
                         source_cells_id )
  call check_err(ierr, routine, cmess)

  cmess = 'Getting '//trim(dim_name)//' value'
  ierr = nf90_inquire_dimension( self%ncid,       &
                                 source_cells_id, &
                                 len=source_ncells )
  call check_err(ierr, routine, cmess)


  ! 2.0 Get number of target cells per source cell
  !=====================================================
  dim_name = 'n'//trim(target_mesh_name)//           &
             '_cells_per_'//trim(source_mesh_name)// &
             '_cell'

  cmess = 'Getting '//trim(dim_name)//' id'
  ierr = nf90_inq_dimid( self%ncid, &
                         dim_name,  &
                         target_cells_per_source_cell_id )
  call check_err(ierr, routine, cmess)

  cmess = 'Getting '//trim(dim_name)//' value'
  ierr = nf90_inquire_dimension( self%ncid,                       &
                                 target_cells_per_source_cell_id, &
                                 len=target_cells_per_source_cell )
  call check_err(ierr, routine, cmess)


  ! 3.0 Allocate array and extract map
  !================================================

  ! 3.1 Allocate the mesh map array to be populated
  if (allocated(mesh_map)) deallocate(mesh_map)
  allocate( mesh_map( target_cells_per_source_cell, source_ncells ))

  ! 3.2 Extract map from the NetCDF file
  cmess = 'Getting '//trim(source_mesh_name)//'-'// &
          trim(target_mesh_name)//' map id'
  var_name = trim(source_mesh_name)//'_'// &
             trim(target_mesh_name)//'_map'

  ierr = nf90_inq_varid( self%ncid, &
                         var_name,  &
                         mesh_map_id )
  call check_err(ierr, routine, cmess)

  cmess = 'Getting '//trim(source_mesh_name)//'-'// &
          trim(target_mesh_name)//' mesh-mesh connectivity'

  ierr = nf90_get_var( self%ncid,   &
                       mesh_map_id, &
                       mesh_map(:,:) )
  call check_err(ierr, routine, cmess)

end subroutine read_map


!-------------------------------------------------------------------------------
!>  @brief Function to determine if mesh is present in NetCDF ugrid file
!>
!>  @param[in]       mesh_name   Name of the mesh topology
!>  @return          answer      .True. if mesh_name is present in file
!-------------------------------------------------------------------------------
function is_mesh_present(self, mesh_name) result(answer)

  implicit none

  class(ncdf_quad_type), intent(in) :: self
  character(str_def),    intent(in) :: mesh_name

  logical(l_def) :: answer

  character(nf90_max_name), allocatable :: mesh_names(:)

  integer(i_def) :: n_meshes
  integer(i_def) :: i

  n_meshes = self%get_n_meshes()
  allocate(mesh_names(n_meshes))
  mesh_names = ''

  call get_mesh_names(self, mesh_names)

  answer = .false.
  do i=1, n_meshes
    if ( trim(mesh_names(i)) == trim(mesh_name) ) then
      answer = .true.
      exit
    end if
  end do

  deallocate(mesh_names)

  return

end function is_mesh_present

!-------------------------------------------------------------------------------
!>  @brief   Returns the NetCDF variable names in the NetCDF file which are
!>           ugrid mesh topologies.
!>  @details Scans the variable attributes for cf_role = mesh_topology as
!>           a flag that the variable name relates to a mesh.
!>
!>  @param[in]  self        ncdf_quad_type object associated with NetCDF file
!>
!>  @param[out] mesh_names  Character[:], Names of the mesh_topologies
!>                          in the NetCDF file
!>  @param[out] n_meshes    Integer, The number of mesh topologies
!>                          in the NetCDf file <<optional>>
!-------------------------------------------------------------------------------
subroutine scan_for_topologies(self, mesh_names, n_meshes)

  implicit none

  class(ncdf_quad_type), intent(in)  :: self
  character(len=*),      intent(out) :: mesh_names(:)
  integer(i_def),        intent(out) :: n_meshes

  character(nf90_max_name), allocatable :: var_names(:)
  integer(i_def),           allocatable :: var_n_attributes(:)
  logical,                  allocatable :: is_mesh_topology(:)

  integer(i_def) :: n_variables
  integer(i_def) :: i, j, counter, ierr

  character(nf90_max_name) :: attribute_name
  character(nf90_max_name) :: attribute_value

  integer(i_def) :: n_mesh_topologies

  character(*), parameter :: routine = 'get_mesh_names'
  character(str_long) :: cmess


  cmess = 'Requesting number of variables'
  ierr = nf90_inquire(ncid=self%ncid, nvariables=n_variables )
  call check_err(ierr, routine, cmess)

  allocate(var_names(n_variables))
  allocate(var_n_attributes(n_variables))
  allocate(is_mesh_topology(n_variables))

  is_mesh_topology = .false.

  do i=1, n_variables
    ierr = nf90_inquire_variable( ncid=self%ncid, varid=i, &
                                  name=var_names(i),       &
                                  natts=var_n_attributes(i) )
    write(cmess,'(2(A,I0))')       &
        'Invalid variable id:', i, &
        'or NetCDF file id:', self%ncid
    call check_err (ierr, routine, cmess)
    do j=1, var_n_attributes(i)
      ierr = nf90_inq_attname( ncid=self%ncid,    &
                               varid=i, attnum=j, &
                               name=attribute_name )
      write(cmess,'(A,I0,A)')              &
          'Invalid attribute number: ', j, &
          ' for variable '//trim(var_names(i))
      call check_err (ierr, routine, cmess)

      if (trim(attribute_name) == 'cf_role') then
        ierr = nf90_get_att( ncid=self%ncid,            &
                             varid=i,                   &
                             name=trim(attribute_name), &
                             values=attribute_value )
        write(cmess,'(A)')                                             &
            'Unable to get value of attribute cf_role for variable '// &
            trim(var_names(i))
        call check_err (ierr, routine, cmess)
        if (trim(attribute_value) == 'mesh_topology') then
          is_mesh_topology(i) = .true.
          exit
        end if
      end if

    end do

  end do

  n_mesh_topologies = count(is_mesh_topology)

  if ( size( mesh_names ) < n_mesh_topologies ) then
    write(log_scratch_space,'(I0,A,I0)') n_mesh_topologies,   &
        ' mesh topologies found but output array provided'//  &
        ' is only length ', size(mesh_names)
    call log_event(trim(log_scratch_space), LOG_LEVEL_ERROR)
  end if

  counter=0
  do i=1, n_variables
    if (is_mesh_topology(i)) then
      counter=counter+1
      mesh_names(counter) = trim(var_names(i))
    end if
  end do

  n_meshes = n_mesh_topologies

  deallocate(var_names)
  deallocate(var_n_attributes)
  deallocate(is_mesh_topology)

  return
end subroutine scan_for_topologies

end module ncdf_quad_mod

