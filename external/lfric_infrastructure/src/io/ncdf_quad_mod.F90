!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!>  @brief   File handler for NetCDF ugrid files.
!>  @details Implementation of the ugrid file class for quads in NetCDF format.
!-------------------------------------------------------------------------------
module ncdf_quad_mod

use constants_mod,  only: r_def, i_def, l_def, str_def, str_long,         &
                          str_longlong, str_max_filename, r_ncdf, i_ncdf, &
                          rmdi, imdi, cmdi

use global_mesh_map_collection_mod, only: global_mesh_map_collection_type
use global_mesh_map_mod,            only: global_mesh_map_type
use local_mesh_map_collection_mod,  only: local_mesh_map_collection_type
use local_mesh_map_mod,             only: local_mesh_map_type

use log_mod, only: log_event, log_scratch_space, log_level_error, &
                   log_level_warning
use netcdf,  only: nf90_max_name, nf90_open, nf90_write, nf90_nowrite,    &
                   nf90_noerr, nf90_strerror, nf90_put_var, nf90_get_var, &
                   nf90_get_att, nf90_inquire, nf90_inquire_variable,     &
                   nf90_def_var, nf90_inq_varid, nf90_int, nf90_double,   &
                   nf90_clobber, nf90_enddef, nf90_inquire_dimension,     &
                   nf90_inq_dimid, nf90_def_dim, nf90_create,             &
                   nf90_inq_attname, nf90_inquire_attribute,              &
                   nf90_redef, nf90_close, nf90_put_att, nf90_64bit_offset

use ugrid_file_mod, only: ugrid_file_type
use file_mod,       only: file_mode_read, file_mode_write

implicit none

private

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------

integer(i_def), parameter :: ONE  = 1   !< One
integer(i_def), parameter :: TWO  = 2   !< Two
integer(i_def), parameter :: FOUR = 4   !< Four

! Ranks for each variable.
integer(i_def), parameter :: MESH_RANK            = 0
integer(i_def), parameter :: MESH_FACE_NODES_RANK = 2  !< Rank of face-node connectivity arrays
integer(i_def), parameter :: MESH_EDGE_NODES_RANK = 2  !< Rank of edge-node connectivity arrays
integer(i_def), parameter :: MESH_FACE_EDGES_RANK = 2  !< Rank of face-edge connectivity arrays
integer(i_def), parameter :: MESH_FACE_LINKS_RANK = 2  !< Rank of face-face connectivity arrays
integer(i_def), parameter :: MESH_MESH_LINKS_RANK = 3  !< Rank of mesh-mesh connectivity arrays
integer(i_def), parameter :: MESH_NODE_X_RANK     = 1  !< Rank of node x coordinate array
integer(i_def), parameter :: MESH_NODE_Y_RANK     = 1  !< Rank of node y coordinate array
integer(i_def), parameter :: MESH_FACE_X_RANK     = 1  !< Rank of face x coordinate array
integer(i_def), parameter :: MESH_FACE_Y_RANK     = 1  !< Rank of face y coordinate array
integer(i_def), parameter :: MESH_FACE_RANK       = 1  !< Rank of face arrays
integer(i_def), parameter :: MESH_NODE_RANK       = 1  !< Rank of node arrays
integer(i_def), parameter :: MESH_EDGE_RANK       = 1  !< Rank of edge arrays
integer(i_def), parameter :: MESH_HALO_RANK       = 1  !< Rank of halo arrays

! Note: For spherical coordinates x is equivalent to longitude
!                                 y is equivalent to latitude

integer(i_def), parameter :: GLOBAL_MESH_FLAG = 15
integer(i_def), parameter :: LOCAL_MESH_FLAG  = 11

integer(i_def), parameter :: GLOBAL_MODEL_FLAG = 136
integer(i_def), parameter :: REGION_MODEL_FLAG = 172

!-------------------------------------------------------------------------------
!> @brief   NetCDF quad file type
!> @details Implements the ugrid file type for NetCDF files storing 2D quads.
!-------------------------------------------------------------------------------

type, extends(ugrid_file_type), public :: ncdf_quad_type

  private

  !> @todo Some data variables introduced here were to expedite OP-PoC
  !>       Mesh data should be passed for reading and writing but not held
  !>       in <ncdf_quad_type>. This should be corrected with Ticket #3493.

  integer(i_def)              :: ncid       !< NetCDF file ID
  character(str_max_filename) :: file_name  !< Filename
  integer(i_def)              :: model_extents = imdi
  integer(i_def)              :: mesh_extents  = imdi

  ! Common mesh type data
  character(nf90_max_name) :: mesh_name
  character(nf90_max_name) :: geometry
  character(nf90_max_name) :: coord_sys
  character(str_def)       :: coord_units_x = cmdi
  character(str_def)       :: coord_units_y = cmdi
  integer(i_def)           :: npanels       = imdi

  character(str_longlong)  :: constructor_inputs = cmdi
                                                 !< Inputs to ugrid_generator
                                                 !< for this mesh

  real(r_def) :: north_pole(2)  = [rmdi, rmdi]   !< [Longitude,Latitude] of
                                                 !< north pole for domain
                                                 !< orientation (degrees)
  real(r_def) :: null_island(2) = [rmdi, rmdi]   !< [Longitude,Latitude] of
                                                 !< null island for domain
                                                 !< orientation (degrees)
  real(r_def) :: equatorial_latitude = rmdi      !< Latitude of equator (degrees)

  ! Dimension values
  integer(i_def) :: nmesh_nodes   = imdi  !< Number of nodes
  integer(i_def) :: nmesh_edges   = imdi  !< Number of edges
  integer(i_def) :: nmesh_faces   = imdi  !< Number of faces
  integer(i_def) :: nmesh_targets = imdi  !< Number of mesh(es) to map to

  ! Partition data
  character(str_def) :: partition_of = cmdi

  integer(i_def) :: inner_depth = imdi
  integer(i_def) :: halo_depth  = imdi
  integer(i_def) :: num_edge    = imdi
  integer(i_def) :: num_ghost   = imdi

  integer(i_def) :: max_stencil_depth  = imdi
  integer(i_def) :: last_edge_cell     = imdi
  integer(i_def) :: last_ghost_cell    = imdi
  integer(i_def) :: nmesh_faces_global = imdi

  integer(i_def), allocatable :: num_inner(:), last_inner_cell(:)
  integer(i_def), allocatable :: num_halo(:),  last_halo_cell(:)
  integer(i_def), allocatable :: node_cell_owner(:)
  integer(i_def), allocatable :: edge_cell_owner(:)

  ! Global metadata
  character(str_def) :: global_var     = cmdi
  integer(i_def)     :: rim_depth      = imdi
  logical(l_def)     :: periodic_xy(2) = .false.
  real(r_def)        :: domain_extents(2,4) = rmdi


  character(nf90_max_name)    :: topology
  integer(i_def), allocatable :: cell_gid(:)
  integer(i_def), allocatable :: node_on_cell_gid(:,:)
  integer(i_def), allocatable :: edge_on_cell_gid(:,:)

  ! InterGrid map variables
  character(nf90_max_name), allocatable :: target_mesh_names(:)
  type(global_mesh_map_collection_type), pointer :: target_global_mesh_maps => null()
  type(local_mesh_map_collection_type),  pointer :: target_local_mesh_maps  => null()

  !==============================
  ! NetCDF id variables
  !==============================

  ! Dimension ids
  integer(i_def) :: one_dim_id           !< NetCDF-assigned ID for constant one
  integer(i_def) :: two_dim_id           !< NetCDF-assigned ID for constant two
  integer(i_def) :: four_dim_id          !< NetCDF-assigned ID for constant four
  integer(i_def) :: nmesh_nodes_dim_id   !< NetCDF-assigned ID for number of nodes
  integer(i_def) :: nmesh_edges_dim_id   !< NetCDF-assigned ID for number of edges
  integer(i_def) :: nmesh_faces_dim_id   !< NetCDF-assigned ID for number of faces

  integer(i_def) :: nmesh_map_faces_dim_id !< number of faces in mesh which have
                                           !< an integrid map

  integer(i_def), allocatable :: ntargets_per_source_dim_id(:)
                                         !< NetCDF-assigned ID for number of mesh targets
  integer(i_def), allocatable :: ntargets_per_source_x_dim_id(:)
                                         !< NetCDF-assigned ID for number of mesh targets in x-dir
  integer(i_def), allocatable :: ntargets_per_source_y_dim_id(:)
                                         !< NetCDF-assigned ID for number of mesh targets in y-dir

  ! Variable ids
  integer(i_def) :: mesh_id              !< NetCDF-assigned ID this mesh
  integer(i_def) :: mesh_face_nodes_id   !< NetCDF-assigned ID for the face-node connectivity
  integer(i_def) :: mesh_face_edges_id   !< NetCDF-assigned ID for the face-edge connectivity
  integer(i_def) :: mesh_face_links_id   !< NetCDF-assigned ID for the face-face connectivity
  integer(i_def) :: mesh_edge_nodes_id   !< NetCDF-assigned ID for the edge-node connectivity
  integer(i_def) :: mesh_node_x_id       !< NetCDF-assigned ID for node x-coordinates
  integer(i_def) :: mesh_node_y_id       !< NetCDF-assigned ID for node y-coordinates
  integer(i_def) :: mesh_face_x_id       !< NetCDF-assigned ID for cell x-coordinates
  integer(i_def) :: mesh_face_y_id       !< NetCDF-assigned ID for cell y-coordinates

  integer(i_def), allocatable :: mesh_mesh_links_id(:)
                                         !< NetCDF-assigned ID for the mesh-mesh connectivity

  integer(i_def) :: partition_id
  integer(i_def) :: node_cell_owner_id
  integer(i_def) :: edge_cell_owner_id
  integer(i_def) :: num_inner_id
  integer(i_def) :: num_halo_id
  integer(i_def) :: last_inner_cell_id
  integer(i_def) :: last_halo_cell_id
  integer(i_def) :: n_inner_dim_id
  integer(i_def) :: n_halos_dim_id
  integer(i_def) :: global_mesh_id
  integer(i_def) :: global_cells_id
  integer(i_def) :: global_face_node_id
  integer(i_def) :: global_face_edge_id
  integer(i_def) :: domain_extents_id

  integer(i_def) :: void_cell

contains

  procedure :: read_mesh
  procedure :: read_map
  procedure :: write_mesh
  procedure :: append_mesh
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
!>  @brief Open an existing NetCDF file to read only.
!>  @param[in] file_name  Name of the file to open.
!>  @param[in] file_mode  Optional, Read/Write Mode, Default(Read).
!-------------------------------------------------------------------------------
subroutine file_open(self, file_name, file_mode)

  implicit none

  ! Arguments
  class(ncdf_quad_type),    intent(inout) :: self
  character(len=*),         intent(in)    :: file_name
  integer(i_def), optional, intent(in)    :: file_mode

  ! Internal variables
  integer(i_def) :: ierr
  character(*), parameter :: routine = 'file_open'
  character(str_long) :: cmess

  integer(i_def) :: mode

  self%file_name = file_name
  cmess = 'Opening file "'//trim(self%file_name)//'" '
  if (present(file_mode)) then

    select case(file_mode)
    case (file_mode_read)
      mode = nf90_nowrite
      write(cmess,'(A)') trim(cmess) // '(read-only)'
    case (file_mode_write)
      mode = nf90_write
      write(cmess,'(A)') trim(cmess) // '(read-write)'
    case default
      write(log_scratch_space,'(A)')            &
          'Invalid requested file_mode for ' // &
          trim(self%file_name)
      call log_event(log_scratch_space, log_level_error)
    end select

  else
    mode = nf90_nowrite
    write(cmess,'(A)') trim(cmess) // '(read-only)'
  end if

  ierr = nf90_open( trim(self%file_name), mode, self%ncid )
  call check_err(ierr, routine, cmess)

  return
end subroutine file_open

!-------------------------------------------------------------------------------
!>  @brief Closes a NetCDF file.
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
!>  @param[in] file_name The name of the file to create.
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
!>  @brief   Defines NetCDF dimensions in the NetCDF file.
!>  @details Sets dimension lengths in the NetCDF file, and sets the associated
!>           dimension ids in the NetCDF file object. The dimension lengths are
!>           used for sizes of other arrays within the NetCDF file.
!-------------------------------------------------------------------------------
subroutine define_dimensions(self)

  implicit none

  ! Arguments
  class(ncdf_quad_type), intent(inout) :: self

  ! Internal variables
  integer(i_def) :: ierr, i, source_id

  character(*), parameter :: routine = 'define_dimensions'
  character(str_long) :: cmess
  character(str_long) :: dim_name
  character(nf90_max_name) :: target_mesh_name

  type(global_mesh_map_type), pointer :: global_mesh_map => null()
  type(local_mesh_map_type),  pointer :: local_mesh_map  => null()

  ! If the file is being appended to, constants may already exist
  ! in the NetCDF file. Trying to redefine the same variable name
  ! will throw a error, so check to see if constants are present
  ! already.
  ierr = nf90_inq_dimid (self%ncid, 'One', self%one_dim_id)
  if (ierr /= nf90_noerr) then
    ierr = nf90_def_dim(self%ncid, 'One', ONE, self%one_dim_id)
    cmess = 'Defining One'
    call check_err(ierr, routine, cmess)
  end if

  ierr = nf90_inq_dimid (self%ncid, 'Two', self%two_dim_id)
  if (ierr /= nf90_noerr) then
    ierr = nf90_def_dim(self%ncid, 'Two', TWO, self%two_dim_id)
    cmess = 'Defining Two'
    call check_err(ierr, routine, cmess)
  end if

  ierr = nf90_inq_dimid(self%ncid, 'Four', self%four_dim_id)
  if (ierr /= nf90_noerr) then
    cmess = 'Defining Four'
    ierr = nf90_def_dim(self%ncid, 'Four', FOUR, self%four_dim_id)
    call check_err(ierr, routine, cmess)
  end if


  ! Define dimensions connected to the mesh
  dim_name = 'n'//trim(self%mesh_name)//'_node'
  cmess = 'Defining '//trim(dim_name)
  ierr = nf90_def_dim( self%ncid, trim(dim_name), &
                       self%nmesh_nodes, self%nmesh_nodes_dim_id )
  call check_err(ierr, routine, cmess)

  dim_name = 'n'//trim(self%mesh_name)//'_edge'
  cmess = 'Defining '//trim(dim_name)
  ierr = nf90_def_dim( self%ncid, trim(dim_name), &
                       self%nmesh_edges,self%nmesh_edges_dim_id )
  call check_err(ierr, routine, cmess)

  dim_name = 'n'//trim(self%mesh_name)//'_face'
  cmess = 'Defining '//trim(dim_name)
  ierr = nf90_def_dim( self%ncid, trim(dim_name), &
                       self%nmesh_faces, self%nmesh_faces_dim_id )
  call check_err(ierr, routine, cmess)


  ! Define dimensions required for each of the cell maps
  ! n<target mesh name> cells per <source mesh name> cell
  source_id = 1
  select case( self%mesh_extents )

  case( GLOBAL_MESH_FLAG )

    ! Global mesh intergrid maps
    !---------------------------
    if (self%nmesh_targets > 0) then
      dim_name = 'n'//trim(self%mesh_name)//'_map_face'
      cmess    = 'Defining '//trim(dim_name)
      ierr     = nf90_def_dim( self%ncid, trim(dim_name), &
                               self%nmesh_faces,          &
                               self%nmesh_map_faces_dim_id )
      call check_err(ierr, routine, cmess)
    end if

    do i=1, self%nmesh_targets

      nullify(global_mesh_map)
      target_mesh_name = self%target_mesh_names(i)
      dim_name = 'n'//trim(target_mesh_name)// &
                 '_cells_per_'//trim(self%mesh_name)//'_x'

      global_mesh_map => self%target_global_mesh_maps%get_global_mesh_map(source_id,i+1)

      ierr = nf90_inq_dimid( self%ncid, trim(dim_name), &
                             self%ntargets_per_source_x_dim_id(i) )

      if (ierr /= nf90_noerr) then
        ierr = nf90_def_dim( self%ncid, trim(dim_name),                        &
                             global_mesh_map%get_ntarget_cells_per_source_x(), &
                             self%ntargets_per_source_x_dim_id(i) )

        cmess = 'Defining '//trim(dim_name)
        call check_err(ierr, routine, cmess)
      end if

      dim_name = 'n'//trim(target_mesh_name)// &
                 '_cells_per_'//trim(self%mesh_name)//'_y'

      ierr = nf90_inq_dimid( self%ncid, trim(dim_name), &
                             self%ntargets_per_source_y_dim_id(i) )

      if (ierr /= nf90_noerr) then
        ierr = nf90_def_dim( self%ncid, trim(dim_name),                        &
                             global_mesh_map%get_ntarget_cells_per_source_y(), &
                             self%ntargets_per_source_y_dim_id(i) )

        cmess = 'Defining '//trim(dim_name)
        call check_err(ierr, routine, cmess)
      end if

      dim_name = 'n'//trim(target_mesh_name)// &
                 '_cells_per_'//trim(self%mesh_name)//'_cell'

      ierr = nf90_inq_dimid( self%ncid, trim(dim_name), &
                             self%ntargets_per_source_dim_id(i) )

      if (ierr /= nf90_noerr) then
        ierr = nf90_def_dim( self%ncid, trim(dim_name),                           &
                             global_mesh_map%get_ntarget_cells_per_source_cell(), &
                             self%ntargets_per_source_dim_id(i) )

        cmess = 'Defining '//trim(dim_name)
        call check_err(ierr, routine, cmess)
      end if

    end do ! nmesh_targets

  case ( LOCAL_MESH_FLAG )

    ! Local mesh intergrid maps
    !--------------------------
    if (self%nmesh_targets > 0) then
      dim_name = 'n'//trim(self%mesh_name)//'_map_face'
      cmess = 'Defining '//trim(dim_name)
      ierr = nf90_def_dim( self%ncid, trim(dim_name),         &
                           self%nmesh_faces - self%num_ghost, &
                           self%nmesh_map_faces_dim_id )
      call check_err(ierr, routine, cmess)
    end if

    do i=1, self%nmesh_targets
      nullify(local_mesh_map)
      target_mesh_name = self%target_mesh_names(i)
      dim_name = 'n'//trim(target_mesh_name)// &
                 '_cells_per_'//trim(self%mesh_name)//'_x'
      !===============================================================================
      ! Source id when writing meshes are source=1 , other ids are incremental
      !===============================================================================
      local_mesh_map => self%target_local_mesh_maps%get_local_mesh_map(source_id,i+1)

      ierr = nf90_inq_dimid( self%ncid, trim(dim_name), &
                             self%ntargets_per_source_x_dim_id(i) )

      if (ierr /= nf90_noerr) then
        ierr = nf90_def_dim( self%ncid, trim(dim_name),                            &
                             local_mesh_map%get_ntarget_cells_per_source_cell_x(), &
                             self%ntargets_per_source_x_dim_id(i) )

        cmess = 'Defining '//trim(dim_name)
        call check_err(ierr, routine, cmess)
      end if

      dim_name = 'n'//trim(target_mesh_name)// &
                 '_cells_per_'//trim(self%mesh_name)//'_y'

      ierr = nf90_inq_dimid( self%ncid, trim(dim_name), &
                             self%ntargets_per_source_y_dim_id(i) )

      if (ierr /= nf90_noerr) then
        ierr = nf90_def_dim( self%ncid, trim(dim_name),                            &
                             local_mesh_map%get_ntarget_cells_per_source_cell_y(), &
                             self%ntargets_per_source_y_dim_id(i) )

        cmess = 'Defining '//trim(dim_name)
        call check_err(ierr, routine, cmess)
      end if

      dim_name = 'n'//trim(target_mesh_name)// &
                 '_cells_per_'//trim(self%mesh_name)//'_cell'

      ierr = nf90_inq_dimid(self%ncid, trim(dim_name), self%ntargets_per_source_dim_id(i))

      if (ierr /= nf90_noerr) then
        ierr = nf90_def_dim( self%ncid, trim(dim_name),                            &
                             local_mesh_map%get_ntarget_cells_per_source_cell_x()* &
                             local_mesh_map%get_ntarget_cells_per_source_cell_y(), &
                             self%ntargets_per_source_dim_id(i) )

        cmess = 'Defining '//trim(dim_name)
        call check_err(ierr, routine, cmess)
      end if

    end do ! nmesh_targets

    ! Halo dimensions
    !----------------
    if (self%inner_depth > 0) then
      dim_name = 'n'//trim(self%mesh_name)//'_inner'

      ierr = nf90_inq_dimid( self%ncid, trim(dim_name), self%n_inner_dim_id )

      if (ierr /= nf90_noerr) then
        ierr = nf90_def_dim( self%ncid, trim(dim_name), &
                             self%inner_depth,          &
                             self%n_inner_dim_id )

        cmess = 'Defining '//trim(dim_name)
        call check_err(ierr, routine, cmess)
      end if
    end if

    if (self%halo_depth > 0) then
      dim_name = 'n'//trim(self%mesh_name)//'_halos'

      ierr = nf90_inq_dimid( self%ncid, trim(dim_name), self%n_halos_dim_id )

      if (ierr /= nf90_noerr) then
        ierr = nf90_def_dim( self%ncid, trim(dim_name), &
                             self%halo_depth,           &
                             self%n_halos_dim_id )

        cmess = 'Defining '//trim(dim_name)
        call check_err(ierr, routine, cmess)
      end if
    end if

  end select

  return
end subroutine define_dimensions


!-------------------------------------------------------------------------------
!>  @brief   Defines NetCDF variables in the netCDF file.
!>  @details Tells NetCDF what variables are going to be in the file.
!>           Array lengths are specified via the pre-existing NetCDF dimension
!>           IDs, which were obtained elsewhere in this module.
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

  integer(i_def) :: local_node_cell_owner_dims(MESH_NODE_RANK)
  integer(i_def) :: local_edge_cell_owner_dims(MESH_EDGE_RANK)
  integer(i_def) :: local_inner_dims(MESH_HALO_RANK)
  integer(i_def) :: local_halos_dims(MESH_HALO_RANK)
  integer(i_def) :: global_cell_dims(MESH_FACE_RANK)
  integer(i_def) :: global_face_nodes_dims(MESH_FACE_NODES_RANK)
  integer(i_def) :: global_face_edges_dims(MESH_FACE_EDGES_RANK)
  integer(i_def) :: domain_extents_dims(2)

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
  ierr = nf90_def_var( self%ncid, trim(var_name),      &
                       nf90_int, mesh_face_nodes_dims, &
                       self%mesh_face_nodes_id )
  call check_err(ierr, routine, cmess)

  mesh_face_edges_dims(1) = self%four_dim_id
  mesh_face_edges_dims(2) = self%nmesh_faces_dim_id
  var_name = trim(self%mesh_name)//'_face_edges'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),      &
                       nf90_int, mesh_face_edges_dims, &
                       self%mesh_face_edges_id )
  call check_err(ierr, routine, cmess)

  mesh_face_links_dims(1) = self%four_dim_id
  mesh_face_links_dims(2) = self%nmesh_faces_dim_id
  var_name = trim(self%mesh_name)//'_face_links'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),      &
                       nf90_int, mesh_face_links_dims, &
                       self%mesh_face_links_id )
  call check_err(ierr, routine, cmess)

  mesh_edge_nodes_dims(1) = self%two_dim_id
  mesh_edge_nodes_dims(2) = self%nmesh_edges_dim_id
  var_name = trim(self%mesh_name)//'_edge_nodes'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),      &
                       nf90_int, mesh_edge_nodes_dims, &
                       self%mesh_edge_nodes_id )
  call check_err(ierr, routine, cmess)

  mesh_node_x_dims(1) = self%nmesh_nodes_dim_id
  var_name = trim(self%mesh_name)//'_node_x'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),      &
                       nf90_double, mesh_node_x_dims,  &
                       self%mesh_node_x_id )
  call check_err(ierr, routine, cmess)

  mesh_node_y_dims(1) = self%nmesh_nodes_dim_id
  var_name = trim(self%mesh_name)//'_node_y'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),      &
                       nf90_double, mesh_node_y_dims,  &
                       self%mesh_node_y_id )
  call check_err(ierr, routine, cmess)


  mesh_face_x_dims(1) = self%nmesh_faces_dim_id
  var_name = trim(self%mesh_name)//'_face_x'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),      &
                       nf90_double, mesh_face_x_dims,  &
                       self%mesh_face_x_id )
  call check_err(ierr, routine, cmess)

  mesh_face_y_dims(1) = self%nmesh_faces_dim_id
  var_name = trim(self%mesh_name)//'_face_y'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),      &
                       nf90_double, mesh_face_y_dims,  &
                       self%mesh_face_y_id )
  call check_err(ierr, routine, cmess)

  ! Global mesh domain extents
  domain_extents_dims(1) = self%two_dim_id
  domain_extents_dims(2) = self%four_dim_id
  var_name = trim(self%mesh_name)//'_domain_extents'
  cmess = 'Defining '//trim(var_name)
  ierr = nf90_def_var( self%ncid, trim(var_name),        &
                       nf90_double, domain_extents_dims, &
                       self%domain_extents_id )
  call check_err(ierr, routine, cmess)

  ! Define partition variables if this a local mesh
  if ( self%mesh_extents == LOCAL_MESH_FLAG ) then

    ! Partition_of <global_mesh>
    cmess = 'Defining '//trim(self%partition_of)
    ierr = nf90_def_var( self%ncid, trim(self%partition_of), &
                         nf90_int, zero_sized, self%partition_id )
    call check_err(ierr, routine, cmess)


    ! Node cell owners
    local_node_cell_owner_dims(1) = self%nmesh_nodes_dim_id
    var_name = trim(self%mesh_name)//'_node_cell_owner'
    cmess = 'Defining '//trim(var_name)
    ierr = nf90_def_var( self%ncid, trim(var_name),            &
                         nf90_int, local_node_cell_owner_dims, &
                         self%node_cell_owner_id )
    call check_err(ierr, routine, cmess)


    ! Edge cell owners
    local_edge_cell_owner_dims(1) = self%nmesh_edges_dim_id
    var_name = trim(self%mesh_name)//'_edge_cell_owner'
    cmess = 'Defining '//trim(var_name)
    ierr = nf90_def_var( self%ncid, trim(var_name),            &
                         nf90_int, local_edge_cell_owner_dims, &
                         self%edge_cell_owner_id )
    call check_err(ierr, routine, cmess)


    ! Inner layers
    if (self%inner_depth >0) then

      local_inner_dims(1) = self%n_inner_dim_id

      var_name = trim(self%mesh_name)//'_num_inner'
      cmess = 'Defining '//trim(var_name)
      ierr = nf90_def_var( self%ncid, trim(var_name),  &
                           nf90_int, local_inner_dims, &
                           self%num_inner_id )
      call check_err(ierr, routine, cmess)

      var_name = trim(self%mesh_name)//'_last_inner_cell'
      cmess = 'Defining '//trim(var_name)
      ierr = nf90_def_var( self%ncid, trim(var_name),  &
                           nf90_int, local_inner_dims, &
                           self%last_inner_cell_id )
      call check_err(ierr, routine, cmess)
    end if


    ! Halo layers
    if (self%halo_depth > 0) then
      local_halos_dims(1) = self%n_halos_dim_id

      var_name = trim(self%mesh_name)//'_num_halo'
      cmess = 'Defining '//trim(var_name)
      ierr = nf90_def_var( self%ncid, trim(var_name),  &
                           nf90_int, local_halos_dims, &
                           self%num_halo_id )
      call check_err(ierr, routine, cmess)


      var_name = trim(self%mesh_name)//'_last_halo_cell'
      cmess = 'Defining '//trim(var_name)
      ierr = nf90_def_var( self%ncid, trim(var_name),  &
                           nf90_int, local_halos_dims, &
                           self%last_halo_cell_id )
      call check_err(ierr, routine, cmess)
    end if


    ! Netcdf variable to hold details about the global
    ! mesh of which this local mesh is a partition of.
    var_name = trim(self%mesh_name)//'_global'
    cmess = 'Defining '//trim(var_name)
    ierr = nf90_def_var( self%ncid, trim(var_name), &
                         nf90_int, zero_sized, self%global_mesh_id )
    call check_err(ierr, routine, cmess)

    ! Global cells which this local mesh maps to.
    global_cell_dims(1) = self%nmesh_faces_dim_id
    var_name = trim(self%mesh_name)//'_cell_gids'
    cmess = 'Defining '//trim(var_name)
    ierr = nf90_def_var( self%ncid, trim(var_name),  &
                         nf90_int, global_cell_dims, &
                         self%global_cells_id )
    call check_err(ierr, routine, cmess)

    ! Global ids of nodes on local cells
    global_face_nodes_dims = [self%four_dim_id, self%nmesh_faces_dim_id]
    var_name = trim(self%mesh_name)//'_node_gids_on_cell'
    cmess = 'Defining '//trim(var_name)
    ierr = nf90_def_var( self%ncid, trim(var_name),        &
                         nf90_int, global_face_nodes_dims, &
                         self%global_face_node_id )
    call check_err(ierr, routine, cmess)

    ! Global ids of edges on local cells
    global_face_edges_dims = [self%four_dim_id, self%nmesh_faces_dim_id]
    var_name = trim(self%mesh_name)//'_edge_gids_on_cell'
    cmess = 'Defining '//trim(var_name)
    ierr = nf90_def_var( self%ncid, trim(var_name),        &
                         nf90_int, global_face_edges_dims, &
                         self%global_face_edge_id )
    call check_err(ierr, routine, cmess)

  end if ! LOCAL_MESH_FLAG

  ! Define variables which this mesh maps to
  do i=1, self%nmesh_targets

    var_name=trim(self%mesh_name)//'_'//trim(self%target_mesh_names(i))//'_map'
    cmess = 'Defining '//trim(var_name)

    mesh_mesh_links_dims(1) = self%ntargets_per_source_x_dim_id(i)
    mesh_mesh_links_dims(2) = self%ntargets_per_source_y_dim_id(i)
    mesh_mesh_links_dims(3) = self%nmesh_map_faces_dim_id

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
  character(nf90_max_name) :: var_value

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

  attname = 'geometry'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       trim(self%geometry))
  call check_err(ierr, routine, cmess)

  attname = 'topology'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       trim(self%topology))
  call check_err(ierr, routine, cmess)

  attname = 'coord_sys'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       trim(self%coord_sys) )
  call check_err(ierr, routine, cmess)

  attname = 'periodic_x'
  write(lchar_px, '(L1)') self%periodic_xy(1)
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       trim(adjustl(lchar_px)))
  call check_err(ierr, routine, cmess)

  attname = 'periodic_y'
  write(lchar_py, '(L1)') self%periodic_xy(2)
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

  ! Note: This will be removed in a future ticket
  !       max_stencil_depth is only revelevant for
  !       local meshes, although existing code reads
  !       it in for the global meshes
  !
  attname = 'max_stencil_depth'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       self%max_stencil_depth )
  call check_err(ierr, routine, cmess)

  attname = 'n_mesh_maps'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), &
                       self%nmesh_targets )
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


  if ( self%model_extents == GLOBAL_MODEL_FLAG .and. &
       self%rim_depth > 0 ) then

    attname = 'rim_depth'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(var_name)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), self%rim_depth )
    call check_err(ierr, routine, cmess)

  end if

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

  ! Only present for spherical lon-lat domains
  ! (geometry=spherical, coord_sys=ll)
  if ( trim(self%coord_sys) == 'll' .and. &
       trim(self%geometry)  == 'spherical' ) then
    attname = 'north_pole'
    cmess   = 'Adding global attribute "'//trim(attname)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), &
                         self%north_pole )
    call check_err(ierr, routine, cmess)

    attname = 'null_island'
    cmess   = 'Adding global attribute "'//trim(attname)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), &
                         self%null_island )
    call check_err(ierr, routine, cmess)
  end if

  ! Only present for periodic, spherical domains
  if ( trim(self%geometry)  == 'spherical' .and. &
       trim(self%topology)  == 'periodic' ) then
    attname = 'equatorial_latitude'
    cmess   = 'Adding global attribute "'//trim(attname)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), &
                         self%equatorial_latitude )
    call check_err(ierr, routine, cmess)
  end if

  ! For a local mesh, add information
  ! about the partition and the global mesh it is a part of
  if (self%mesh_extents == LOCAL_MESH_FLAG) then
    attname = 'partition_info'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(var_name)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), &
                         trim(self%partition_of))
    call check_err(ierr, routine, cmess)

    attname = 'global_info'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(var_name)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), &
                         trim(self%mesh_name)//'_global')
    call check_err(ierr, routine, cmess)

  end if

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

  attname = '_FillValue'
  cmess   = 'Adding attribute "'//trim(attname)// &
            '" to variable "'//trim(var_name)//'"'
  ierr = nf90_put_att( self%ncid, id, trim(attname), self%void_cell )
  call check_err(ierr, routine, cmess)

  !===================================================================
  ! 6.0 Add attributes for mesh node coordinate variables
  !===================================================================
  select case (trim(self%coord_sys))
  case ('ll')
    std_x_name  = 'longitude'
    std_y_name  = 'latitude'
    long_x_name = 'longitude of 2D mesh nodes.'
    long_y_name = 'latitude of 2D mesh nodes.'

  case ('xyz')
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
  select case (trim(self%coord_sys))
  case ('ll')
    std_x_name  = 'longitude'
    std_y_name  = 'latitude'
    long_x_name = 'longitude of 2D face centres'
    long_y_name = 'latitude of 2D face centres'
  case ('xyz')
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


  !===================================================================
  ! 9.0 Add attributes for local meshes info.
  !===================================================================
  if ( self%mesh_extents == LOCAL_MESH_FLAG ) then

    !============================================================
    ! 9.1 Add data about local mesh's partition.
    !============================================================
    id = self%partition_id

    attname = 'inner_depth'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), self%inner_depth )
    call check_err(ierr, routine, cmess)

    attname = 'halo_depth'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), self%halo_depth )
    call check_err(ierr, routine, cmess)

    attname = 'n_edge_cells'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), self%num_edge )
    call check_err(ierr, routine, cmess)

    attname = 'n_ghost_cells'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), self%num_ghost )
    call check_err(ierr, routine, cmess)

    attname = 'last_edge_cell_index'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), self%last_edge_cell )
    call check_err(ierr, routine, cmess)

    attname = 'last_ghost_cell_index'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), self%last_ghost_cell )
    call check_err(ierr, routine, cmess)

    attname = 'max_stencil_depth'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), self%max_stencil_depth )
    call check_err(ierr, routine, cmess)

    attname = 'node_owner'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    var_value = trim(self%mesh_name)//'_node_cell_owner'
    ierr = nf90_put_att( self%ncid, id, trim(attname), var_value )
    call check_err(ierr, routine, cmess)

    attname = 'edge_owner'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    var_value = trim(self%mesh_name)//'_edge_cell_owner'
    ierr = nf90_put_att( self%ncid, id, trim(attname), var_value )
    call check_err(ierr, routine, cmess)

    attname = 'num_inner'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    var_value = trim(self%mesh_name)//'_num_inner'
    ierr = nf90_put_att( self%ncid, id, trim(attname), var_value )
    call check_err(ierr, routine, cmess)

    attname = 'num_halo'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    var_value = trim(self%mesh_name)//'_num_halo'
    ierr = nf90_put_att( self%ncid, id, trim(attname), var_value )
    call check_err(ierr, routine, cmess)

    attname = 'last_inner_cell'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    var_value = trim(self%mesh_name)//'_last_inner_cell'
    ierr = nf90_put_att( self%ncid, id, trim(attname), var_value )
    call check_err(ierr, routine, cmess)

    attname = 'last_halo_cell'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%partition_of)//'"'
    var_value = trim(self%mesh_name)//'_last_halo_cell'
    ierr = nf90_put_att( self%ncid, id, trim(attname), var_value )
    call check_err(ierr, routine, cmess)


    !============================================================
    ! 9.2 Add data about local mesh's global parent.
    !============================================================
    id = self%global_mesh_id

    attname = 'constructor_inputs'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(var_name)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), &
                         trim(self%constructor_inputs) )
    call check_err(ierr, routine, cmess)

    attname = 'npanels'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%mesh_name)//'_global"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), self%npanels )
    call check_err(ierr, routine, cmess)

    attname = 'ncells'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%mesh_name)//'_global"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), &
                         self%nmesh_faces_global )
    call check_err(ierr, routine, cmess)

    attname = 'domain_extents'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%mesh_name)//'_global"'
    var_value = trim(self%mesh_name)//'_domain_extents'
    ierr = nf90_put_att( self%ncid, id, trim(attname), var_value )
    call check_err(ierr, routine, cmess)

    attname = 'topology'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(var_name)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), &
                         trim(self%topology))
    call check_err(ierr, routine, cmess)

    attname = 'periodic_x'
    write(lchar_px, '(L1)') self%periodic_xy(1)
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(var_name)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), &
                         trim(adjustl(lchar_px)))
    call check_err(ierr, routine, cmess)

    attname = 'periodic_y'
    write(lchar_py, '(L1)') self%periodic_xy(2)
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(var_name)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), &
                         trim(adjustl(lchar_py)))
    call check_err(ierr, routine, cmess)

    if ( self%model_extents == REGION_MODEL_FLAG ) then
      if ( self%rim_depth > 0 ) then
        attname = 'rim_depth'
        cmess   = 'Adding attribute "'//trim(attname)// &
                  '" to variable "'//trim(self%mesh_name)//'_global"'
        ierr = nf90_put_att( self%ncid, id, trim(attname), self%rim_depth )
        call check_err(ierr, routine, cmess)
      end if
    end if ! REGION_MODEL_FLAG

    attname = 'cells'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%mesh_name)//'_global"'
    var_value = trim(self%mesh_name)//'_cell_gids'
    ierr = nf90_put_att( self%ncid, id, trim(attname), var_value )
    call check_err(ierr, routine, cmess)

    attname = 'nodes_on_cell'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%mesh_name)//'_global"'
    var_value = trim(self%mesh_name)//'_node_gids_on_cell'
    ierr = nf90_put_att( self%ncid, id, trim(attname), var_value )
    call check_err(ierr, routine, cmess)

    attname = 'edges_on_cell'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%mesh_name)//'_global"'
    var_value = trim(self%mesh_name)//'_edge_gids_on_cell'
    ierr = nf90_put_att( self%ncid, id, trim(attname), var_value )
    call check_err(ierr, routine, cmess)

  else

    ! Add attributes to global mesh
    id = self%mesh_id

    attname = 'domain_extents'
    cmess   = 'Adding attribute "'//trim(attname)// &
               '" to variable "'//trim(self%mesh_name)
    var_value = trim(self%mesh_name)//'_domain_extents'
    ierr = nf90_put_att( self%ncid, id, trim(attname), var_value )
    call check_err(ierr, routine, cmess)

    attname = 'npanels'
    cmess   = 'Adding attribute "'//trim(attname)// &
              '" to variable "'//trim(self%mesh_name)//'"'
    ierr = nf90_put_att( self%ncid, id, trim(attname), self%npanels )
    call check_err(ierr, routine, cmess)

  end if ! LOCAL_MESH_FLAG

  return
end subroutine assign_attributes

!-------------------------------------------------------------------------------
!>  @brief   Gets dimension ids and variable ids from the open NetCDF file.
!>  @details NetCDF files refer to dimensions and variables by an id, the value
!>           of which is determined by the NetCDF library. This routine finds
!>           dimension and variable ids for all variables of interest in the
!>           open NetCDF file.
!>
!>  @param[in] mesh_name Name of mesh topology to get ids for.
!-------------------------------------------------------------------------------
subroutine inquire_ids(self, mesh_name)

  implicit none

  ! Arguments
  type(ncdf_quad_type), intent(inout) :: self

  character(str_def), intent(in) :: mesh_name

  ! Internal variables
  integer(i_def) :: ierr, id

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

  ! Check to see if mesh has any intergrid maps.
  ! Need to have a dimension for the number of cells which
  ! have a corresponding cell (integrid map). However this
  ! dimension may not exist i.e. if there are no maps in the file.
  ! In this case we do not want the read to abort.
  ! So we will remove the check_err and set the dimension to 0
  cmess = 'Getting attribute, "n_mesh_maps"'
  ierr = nf90_get_att( self%ncid, self%mesh_id, &
                       'n_mesh_maps', self%nmesh_targets )
  if (ierr /= nf90_noerr) self%nmesh_targets = 0

  ! Check to see if mesh is a local mesh
  if ( is_mesh_local( self, mesh_name ) ) then
    self%mesh_extents = LOCAL_MESH_FLAG
  else
    self%mesh_extents = GLOBAL_MESH_FLAG
    self%partition_of = cmdi
    self%global_var   = cmdi
  end if

  !---------------------------------------------------------------

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

  ! Edge node connectivity
  var_name = trim(mesh_name)//'_edge_nodes'
  cmess = 'Getting id for '//trim(var_name)
  ierr = nf90_inq_varid( self%ncid, trim(var_name), &
                         self%mesh_edge_nodes_id )
  call check_err( ierr, routine, cmess )

  dim_name = 'Two'
  cmess = 'Getting id for '//trim(dim_name)
  ierr = nf90_inq_dimid(self%ncid, trim(dim_name),  self%two_dim_id)
  call check_err(ierr,routine, cmess)

  dim_name = 'Four'
  cmess = 'Getting id for '//trim(dim_name)
  ierr = nf90_inq_dimid(self%ncid, trim(dim_name), self%four_dim_id)
  call check_err(ierr, routine, cmess)

  ! Dimension ID for number of cells with
  ! an intergrid map, if present.
  !-----------------------------------------------
  if (self%nmesh_targets > 0) then
    dim_name = 'n'//trim(mesh_name)//'_map_face'
    cmess = 'Getting id for '//trim(dim_name)
    ierr = nf90_inq_dimid( self%ncid, trim(dim_name), &
                           self%nmesh_map_faces_dim_id )
  end if

  ! For local mesh objects
  !-----------------------------------------------
  if ( self%mesh_extents == LOCAL_MESH_FLAG ) then

    ierr = nf90_get_att( self%ncid, self%mesh_id, &
                         'partition_info', self%partition_of )

    ierr = nf90_get_att( self%ncid, self%mesh_id, &
                         'global_info', self%global_var )
    ierr = nf90_inq_varid( self%ncid, trim(self%partition_of), self%partition_id )
    ierr = nf90_inq_varid( self%ncid, trim(self%global_var),   self%global_mesh_id )


    ! Get IDs of variable related to the partition.
    id = self%partition_id

    ierr = nf90_get_att(   self%ncid, id, 'node_owner', var_name )
    ierr = nf90_inq_varid( self%ncid, trim(var_name),  self%node_cell_owner_id )

    ierr = nf90_get_att(   self%ncid, id, 'edge_owner', var_name )
    ierr = nf90_inq_varid( self%ncid, trim(var_name),  self%edge_cell_owner_id )

    ierr = nf90_get_att(   self%ncid, id, 'num_inner', var_name )
    ierr = nf90_inq_varid( self%ncid, trim(var_name),  self%num_inner_id )

    ierr = nf90_get_att(   self%ncid, id, 'num_halo', var_name )
    ierr = nf90_inq_varid( self%ncid, trim(var_name), self%num_halo_id )

    ierr = nf90_get_att(   self%ncid, id, 'last_inner_cell', var_name )
    ierr = nf90_inq_varid( self%ncid, trim(var_name), self%last_inner_cell_id )

    ierr = nf90_get_att(   self%ncid, id, 'last_halo_cell', var_name )
    ierr = nf90_inq_varid( self%ncid, trim(var_name), self%last_halo_cell_id )


    ! Get IDs of variable related to the global mesh.
    id = self%global_mesh_id

    ierr = nf90_get_att(   self%ncid, id, 'domain_extents', var_name )
    ierr = nf90_inq_varid( self%ncid, trim(var_name), self%domain_extents_id )

    ierr = nf90_get_att(   self%ncid, id, 'cells', var_name )
    ierr = nf90_inq_varid( self%ncid, trim(var_name), self%global_cells_id )

    ierr = nf90_get_att(   self%ncid, id, 'nodes_on_cell', var_name )
    ierr = nf90_inq_varid( self%ncid, trim(var_name), self%global_face_node_id )

    ierr = nf90_get_att(   self%ncid, id, 'edges_on_cell', var_name )
    ierr = nf90_inq_varid( self%ncid, trim(var_name), self%global_face_edge_id )

  else
    id = self%mesh_id
    ierr = nf90_get_att(   self%ncid, id, 'domain_extents', var_name )
    ierr = nf90_inq_varid( self%ncid, trim(var_name), self%domain_extents_id )
  end if

  return
end subroutine inquire_ids

!-------------------------------------------------------------------------------
!>  @brief   Calls logger on error.
!>  @details Checks the error code returned by the NetCDF file. If an error is
!>           detected, the relevant error message is passed to the logger.
!>
!>  @param[in] ierr      The error code to check.
!>  @param[in] routine   The routine name that call the error check
!>  @param[in] cmess     Comment message for the error report
!>  @param(in] log_level [optional] Logging behaviour for this call
!-------------------------------------------------------------------------------
subroutine check_err(ierr, routine, cmess, log_level)

  implicit none

  ! Arguments
  integer(i_def),      intent(in) :: ierr
  character(*),        intent(in) :: routine
  character(str_long), intent(in) :: cmess

  integer(i_def),      intent(in), optional :: log_level

  integer(i_def) :: local_log_level

  if (ierr /= NF90_NOERR) then

    local_log_level = log_level_error
    if (present(log_level)) local_log_level = log_level

    write(log_scratch_space,*)                      &
        'Reported in ncdf_quad ['//routine//']: '// &
        trim(cmess) // ' ' // trim(nf90_strerror(ierr))

    call log_event(log_scratch_space, local_log_level)

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
!>  @param[in]  mesh_name              Name of the mesh topology
!>  @param[out] num_nodes              The number of nodes on the mesh.
!>  @param[out] num_edges              The number of edges on the mesh.
!>  @param[out] num_faces              The number of faces on the mesh.
!>  @param[out] num_nodes_per_face     The number of nodes per face.
!>  @param[out] num_edges_per_face     The number of edges per face.
!>  @param[out] num_nodes_per_edge     The number of nodes per edge.
!>  @param[out] max_num_faces_per_node The maximum number of faces surrounding a node.
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
!> @brief   Read data from the NetCDF file.
!> @details Reads coordinate and connectivity information from the NetCDF file.
!>
!> @param[in]   mesh_name               Name of the mesh topology.
!> @param[out]  geometry                Mesh domain geometry.
!> @param[out]  coord_sys               Co-ordinate system used to convey
!> @param[out]  north_pole              [Longitude, Latitude] of north pole
!>                                      for domain orientation (degrees)
!> @param[out]  null_island             [Longitude, Latitude] of null
!>                                      island for domain orientation (degrees)
!> @param[out]  equatorial_latitude     Latitude of equator of mesh (degrees)
!> @param[out]  node_coordinates        Coordinates of each node.
!> @param[out]  face_coordinates        Coordinates of each face.
!> @param[out]  coord_units_x           Units of x coordinates.
!> @param[out]  coord_units_y           Units of y coordinates.
!> @param[out]  void_cell               Cell ID to mark null connectivity.
!> @param[out]  face_node_connectivity  Nodes adjoining each face.
!> @param[out]  face_edge_connectivity  Edges adjoining each face.
!> @param[out]  face_face_connectivity  Faces adjoining each face (links).
!> @param[out]  edge_node_connectivity  Nodes adjoining each edge.

!> @param[out]  topology            Indicates layout of mesh connectivity
!>                                  node locations.
!> @param[out]  periodic_xy         Periodic in x/y-axes.
!> @param[out]  domain_extents      Principal coordinates that
!>                                  describe the domain shape.
!> @param[out]  npanels             Number of panels in this mesh.
!> @param[out]  rim_depth           Depth in cells of global mesh rim
!>                                  (LBC meshes).
!> @param[out]  constructor_inputs  Inputs to the ugrid_generator to
!>                                  generate mesh.
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
!-------------------------------------------------------------------------------
subroutine read_mesh( self,                                              &

                      ! Common mesh related variables.
                      mesh_name, geometry, coord_sys,                    &
                      north_pole, null_island, equatorial_latitude,      &
                      node_coordinates, face_coordinates,                &
                      coord_units_x, coord_units_y, void_cell,           &
                      face_node_connectivity, face_edge_connectivity,    &
                      face_face_connectivity, edge_node_connectivity,    &

                      ! Global mesh info.
                      topology, periodic_xy, domain_extents,             &
                      npanels, rim_depth, constructor_inputs,            &

                      ! Partition info.
                      partition_of, num_faces_global, max_stencil_depth, &
                      inner_depth, num_inner, last_inner_cell,           &
                      halo_depth,  num_halo,  last_halo_cell,            &
                      num_edge,  last_edge_cell,                         &
                      num_ghost, last_ghost_cell,                        &
                      node_cell_owner, edge_cell_owner,                  &
                      cell_gid, node_on_cell_gid, edge_on_cell_gid,      &

                      ! Intergrid maps
                      num_targets, target_mesh_names )

  implicit none

  ! Arguments
  class(ncdf_quad_type),  intent(inout) :: self

  character(str_def),  intent(in)  :: mesh_name
  character(str_def),  intent(out) :: geometry
  character(str_def),  intent(out) :: coord_sys
  real(r_def),         intent(out) :: north_pole(2)
  real(r_def),         intent(out) :: null_island(2)
  real(r_def),         intent(out) :: equatorial_latitude
  real(r_def),         intent(out) :: node_coordinates(:,:)
  real(r_def),         intent(out) :: face_coordinates(:,:)
  character(str_def),  intent(out) :: coord_units_x
  character(str_def),  intent(out) :: coord_units_y
  integer(i_def),      intent(out) :: void_cell
  integer(i_def),      intent(out) :: face_node_connectivity(:,:)
  integer(i_def),      intent(out) :: face_edge_connectivity(:,:)
  integer(i_def),      intent(out) :: face_face_connectivity(:,:)
  integer(i_def),      intent(out) :: edge_node_connectivity(:,:)

  character(str_def),  intent(out) :: topology
  logical(l_def),      intent(out) :: periodic_xy(2)
  real(r_def),         intent(out) :: domain_extents(2,4)
  integer(i_def),      intent(out) :: npanels
  integer(i_def),      intent(out) :: rim_depth

  character(str_longlong), intent(out) :: constructor_inputs

  character(str_def), intent(out) :: partition_of
  integer(i_def),     intent(out) :: num_faces_global
  integer(i_def),     intent(out) :: max_stencil_depth

  integer(i_def), intent(out) :: inner_depth
  integer(i_def), intent(out) :: halo_depth

  integer(i_def), intent(out), allocatable :: num_inner(:), last_inner_cell(:)
  integer(i_def), intent(out), allocatable :: num_halo(:),  last_halo_cell(:)
  integer(i_def), intent(out)              :: num_edge,     last_edge_cell
  integer(i_def), intent(out)              :: num_ghost,    last_ghost_cell

  integer(i_def), intent(out), allocatable :: node_cell_owner(:)
  integer(i_def), intent(out), allocatable :: edge_cell_owner(:)

  integer(i_def), intent(out), allocatable :: cell_gid(:)
  integer(i_def), intent(out), allocatable :: node_on_cell_gid(:,:)
  integer(i_def), intent(out), allocatable :: edge_on_cell_gid(:,:)

  integer(i_def),     intent(out)              :: num_targets
  character(str_def), intent(out), allocatable :: target_mesh_names(:)


  ! Internal variables
  character(*), parameter :: routine = 'read_mesh'

  character(str_long) :: cmess
  character(str_long) :: target_mesh_names_str
  character(str_long) :: attname

  integer(i_def) :: ierr, upper_bound, i

  ! We need to ensure that netcdf receives data with the appropriate
  ! precision to create temporary arrays to hold real data
  ! converted from/to default precision.
  real(r_ncdf), allocatable :: node_coordinates_ncdf(:,:)
  real(r_ncdf), allocatable :: face_coordinates_ncdf(:,:)

  integer(i_def)  :: lower1, upper1
  integer(i_def)  :: lower2, upper2
  integer(i_def)  :: id
  integer(i_ncdf) :: arr_len

  real(r_ncdf), allocatable :: north_pole_ncdf(:)
  real(r_ncdf), allocatable :: null_island_ncdf(:)
  real(r_ncdf)              :: equatorial_latitude_ncdf

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

  call inquire_ids( self, mesh_name )

  partition_of = trim(self%partition_of)

  ! Mesh geometry
  cmess = 'Getting attribute, "geometry"'
  ierr  = nf90_get_att( self%ncid, self%mesh_id, &
                        'geometry', geometry )
  call check_err(ierr, routine, cmess)

  ! Mesh topology
  cmess = 'Getting attribute, "topology"'
  ierr  = nf90_get_att( self%ncid, self%mesh_id, &
                        'topology', topology )
  call check_err(ierr, routine, cmess)

  ! Coordinate system
  cmess = 'Getting attribute, "coord_sys"'
  ierr  = nf90_get_att( self%ncid, self%mesh_id, &
                        'coord_sys', coord_sys )
  call check_err(ierr, routine, cmess)

  ! Periodic in E-W direction
  cmess = 'Getting attribute, "periodic_x"'
  ierr  = nf90_get_att( self%ncid, self%mesh_id, &
                        'periodic_x', lchar_px )
  call check_err(ierr, routine, cmess)
  read(lchar_px, '(L8)') periodic_xy(1)

  ! Periodic in N-S direction
  cmess = 'Getting attribute, "periodic_y"'
  ierr  = nf90_get_att( self%ncid, self%mesh_id, &
                        'periodic_y', lchar_py )
  call check_err(ierr, routine, cmess)
  read(lchar_py, '(L8)') periodic_xy(2)

  ! Number of mesh maps
  cmess = 'Getting attribute, "n_mesh_maps"'
  ierr  = nf90_get_att( self%ncid, self%mesh_id, &
                        'n_mesh_maps', num_targets )
  call check_err(ierr, routine, cmess)
  self%nmesh_targets = num_targets

  ! Read in names of the target meshes to map to.
  ! Can't read in the actual maps as they require
  ! the global mesh ids of the source and target.
  ! The target may not have been read in at this point.
  ! So target names are read, and the maps read in after
  ! global meshes have been read in.

  ! Integer global mesh IDs assigned by the global_mesh_mod/collection
  if (self%nmesh_targets > 0 ) then

    allocate(target_mesh_names(self%nmesh_targets))

    ! 1.0 Find out the target mesh names
    ierr = nf90_inquire_attribute(self%ncid, self%mesh_id, "maps_to")
    if (ierr == nf90_noerr) then

      cmess = 'Getting attribute, "maps_to"'
      ierr  = nf90_get_att( self%ncid, self%mesh_id, &
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
  ierr  = nf90_get_att( self%ncid, self%mesh_node_x_id, "units", coord_units_x )
  call check_err(ierr, routine, cmess)

  cmess = 'Getting y-coord units for mesh "'//trim(mesh_name)//'"'
  ierr  = nf90_get_att( self%ncid, self%mesh_node_y_id, "units", coord_units_y )
  call check_err(ierr, routine, cmess)


  ! Node coordinates
  cmess = 'Getting node x coords for mesh "'//trim(mesh_name)//'"'
  ierr  = nf90_get_var( self%ncid, self%mesh_node_x_id, &
                        node_coordinates_ncdf(1,:))
  call check_err(ierr, routine, cmess)

  cmess = 'Getting node y coords for mesh "'//trim(mesh_name)//'"'
  ierr  = nf90_get_var( self%ncid, self%mesh_node_y_id, &
                        node_coordinates_ncdf(2,:))
  call check_err(ierr, routine, cmess)


  ! Face coordinates
  cmess = 'Getting face x coords for mesh "'//trim(mesh_name)//'"'
  ierr  = nf90_get_var( self%ncid, self%mesh_face_x_id, &
                        face_coordinates_ncdf(1,:))
  call check_err(ierr, routine, cmess)

  cmess = 'Getting face y coords for mesh "'//trim(mesh_name)//'"'
  ierr  = nf90_get_var( self%ncid, self%mesh_face_y_id, &
                        face_coordinates_ncdf(2,:))
  call check_err(ierr, routine, cmess)


  ! Face-Node connectivity
  cmess = 'Getting face-node connectivity for mesh "'//trim(mesh_name)//'"'
  ierr  = nf90_get_var( self%ncid, self%mesh_face_nodes_id, &
                        face_node_connectivity(:,:) )
  call check_err(ierr, routine, cmess)


  ! Face-Edge connectivity
  cmess = 'Getting face-edge connectivity for mesh "'//trim(mesh_name)//'"'
  ierr  = nf90_get_var( self%ncid, self%mesh_face_edges_id, &
                        face_edge_connectivity(:,:) )
  call check_err(ierr, routine, cmess)


  ! Face-Face connectivity
  cmess = 'Getting face-face connectivity for mesh "'//trim(mesh_name)//'"'
  ierr  = nf90_get_var( self%ncid, self%mesh_face_links_id, &
                        face_face_connectivity(:,:))
  call check_err(ierr, routine, cmess)

  attname = '_FillValue'
  cmess = 'Getting _FillValue for mesh "'//trim(mesh_name)//'" cell-cell connectivity'
  ierr  = nf90_inquire_attribute(self%ncid, self%mesh_face_links_id, trim(attname))
  call check_err(ierr, routine, cmess)
  if (ierr == NF90_NOERR) then
    ierr = nf90_get_att(self%ncid, self%mesh_face_links_id, trim(attname), self%void_cell)
    void_cell = self%void_cell
  end if


  ! Edge-Node connectivity
  cmess = 'Getting edge-node connectivity for mesh "'//trim(mesh_name)//'"'
  ierr  = nf90_get_var( self%ncid, self%mesh_edge_nodes_id, &
                        edge_node_connectivity(:,:) )
  call check_err(ierr, routine, cmess)


  ! Only present for spherical lon-lat domains,
  ! (geometry=spherical, coord_sys=ll)
  if ( trim(coord_sys) == 'll' .and. &
       trim(geometry)  == 'spherical' ) then

    attname = 'north_pole'
    cmess = 'Getting North Pole for mesh "'//trim(mesh_name)//'"'
    ierr  = nf90_inquire_attribute(self%ncid, self%mesh_id, trim(attname), len=arr_len)
    call check_err(ierr, routine, cmess, log_level=log_level_warning)
    if (ierr == NF90_NOERR) then
      allocate(north_pole_ncdf(arr_len))
      ierr = nf90_get_att(self%ncid, self%mesh_id, trim(attname), north_pole_ncdf)
      north_pole(1:2) = real(north_pole_ncdf(1:2), kind=r_def)
    end if

    attname = 'null_island'
    cmess = 'Getting Null Island for mesh "'//trim(mesh_name)//'"'
    ierr  = nf90_inquire_attribute(self%ncid, self%mesh_id, trim(attname), len=arr_len)
    call check_err(ierr, routine, cmess, log_level=log_level_warning)
    if (ierr == NF90_NOERR) then
      allocate(null_island_ncdf(arr_len))
      ierr = nf90_get_att(self%ncid, self%mesh_id, trim(attname), null_island_ncdf)
      null_island(1:2) = real(null_island_ncdf(1:2), kind=r_def)
    end if

  end if


  ! Only present for spherical periodic domains,
  if ( trim(topology) == 'periodic' .and. &
       trim(geometry)  == 'spherical' ) then

    attname = 'equatorial_latitude'
    cmess = 'Getting latitude of equator of mesh "'//trim(mesh_name)//'"'
    ierr  = nf90_inquire_attribute(self%ncid, self%mesh_id, trim(attname), len=arr_len)
    call check_err(ierr, routine, cmess, log_level=log_level_warning)
    if (ierr == NF90_NOERR) then
      ierr = nf90_get_att(self%ncid, self%mesh_id, trim(attname), equatorial_latitude_ncdf)
      equatorial_latitude = real(equatorial_latitude_ncdf, kind=r_def)
    end if

  end if

  ! Pass back to r_def arrays and deallocate
  node_coordinates(:,:) = real( node_coordinates_ncdf(:,:), kind=r_def )
  face_coordinates(:,:) = real( face_coordinates_ncdf(:,:), kind=r_def )
  deallocate(node_coordinates_ncdf)
  deallocate(face_coordinates_ncdf)

  select case ( self%mesh_extents )

  case ( LOCAL_MESH_FLAG )

    id = self%partition_id

    ! Purge arrays if already allocated
    if ( allocated( node_cell_owner  ) ) deallocate( node_cell_owner )
    if ( allocated( edge_cell_owner  ) ) deallocate( edge_cell_owner )
    if ( allocated( num_inner ) )        deallocate( num_inner )
    if ( allocated( num_halo  ) )        deallocate( num_halo )
    if ( allocated( last_inner_cell ) )  deallocate( last_inner_cell )
    if ( allocated( last_halo_cell ) )   deallocate( last_halo_cell )

    if ( allocated( cell_gid  ) )        deallocate( cell_gid )
    if ( allocated( node_on_cell_gid ) ) deallocate( node_on_cell_gid )
    if ( allocated( edge_on_cell_gid ) ) deallocate( edge_on_cell_gid )


    !====================================================
    ! Read in information on the local PARTITION variable
    !====================================================
    attname = 'inner_depth'
    ierr = nf90_get_att( self%ncid, id, trim(attname), &
                         self%inner_depth )
    if (ierr == NF90_NOERR) inner_depth = self%inner_depth

    attname = 'halo_depth'
    ierr = nf90_get_att( self%ncid, id, trim(attname), &
                         self%halo_depth )
    if (ierr == NF90_NOERR) halo_depth = self%halo_depth

    attname = 'n_edge_cells'
    ierr = nf90_get_att( self%ncid, id, trim(attname), &
                         self%num_edge )
    if (ierr == NF90_NOERR) num_edge = self%num_edge

    attname = 'n_ghost_cells'
    ierr = nf90_get_att( self%ncid, id, trim(attname), &
                         self%num_ghost )
    if (ierr == NF90_NOERR) num_ghost = self%num_ghost

    attname = 'last_edge_cell_index'
    ierr = nf90_get_att( self%ncid, id, trim(attname), &
                         self%last_edge_cell )
    if (ierr == NF90_NOERR) last_edge_cell = self%last_edge_cell

    attname = 'last_ghost_cell_index'
    ierr = nf90_get_att( self%ncid, id, trim(attname), &
                         self%last_ghost_cell )
    if (ierr == NF90_NOERR) last_ghost_cell = self%last_ghost_cell

    attname = 'max_stencil_depth'
    ierr = nf90_get_att( self%ncid, id, trim(attname), &
                         self%max_stencil_depth )
    if (ierr == NF90_NOERR) max_stencil_depth = self%max_stencil_depth

    ! Allocate arrays to read into
    if ( allocated(self%node_cell_owner) )  deallocate( self%node_cell_owner )
    if ( allocated(self%edge_cell_owner) )  deallocate( self%edge_cell_owner )
    if ( allocated(self%num_inner) )        deallocate( self%num_inner )
    if ( allocated(self%num_halo) )         deallocate( self%num_halo )
    if ( allocated(self%last_inner_cell) )  deallocate( self%last_inner_cell )
    if ( allocated(self%last_halo_cell) )   deallocate( self%last_halo_cell )
    if ( allocated(self%cell_gid) )         deallocate( self%cell_gid )
    if ( allocated(self%node_on_cell_gid) ) deallocate( self%node_on_cell_gid )
    if ( allocated(self%edge_on_cell_gid) ) deallocate( self%edge_on_cell_gid )

    allocate( self%node_cell_owner  (self%nmesh_nodes)   )
    allocate( self%edge_cell_owner  (self%nmesh_edges)   )
    allocate( self%num_inner        (self%inner_depth)   )
    allocate( self%num_halo         (self%halo_depth)    )
    allocate( self%last_inner_cell  (self%inner_depth)   )
    allocate( self%last_halo_cell   (self%halo_depth)    )
    allocate( self%cell_gid         (self%nmesh_faces)   )
    allocate( self%node_on_cell_gid (4,self%nmesh_faces) )
    allocate( self%edge_on_cell_gid (4,self%nmesh_faces) )


    ierr = nf90_get_var( self%ncid,               &
                         self%node_cell_owner_id, &
                         self%node_cell_owner(:) )
    if (ierr == NF90_NOERR) then
      allocate(node_cell_owner, source=self%node_cell_owner)
    end if

    ierr = nf90_get_var( self%ncid,               &
                         self%edge_cell_owner_id, &
                         self%edge_cell_owner(:) )
    if (ierr == NF90_NOERR) then
      allocate(edge_cell_owner, source=self%edge_cell_owner)
    end if

    ierr = nf90_get_var( self%ncid,         &
                         self%num_inner_id, &
                         self%num_inner(:) )
    if (ierr == NF90_NOERR) then
      allocate(num_inner, source=self%num_inner)
    end if

    ierr = nf90_get_var( self%ncid,        &
                         self%num_halo_id, &
                         self%num_halo(:) )
    if (ierr == NF90_NOERR) then
      allocate(num_halo, source=self%num_halo)
    end if

    ierr = nf90_get_var( self%ncid,               &
                         self%last_inner_cell_id, &
                         self%last_inner_cell(:) )
    if (ierr == NF90_NOERR) then
      allocate(last_inner_cell, source=self%last_inner_cell)
    end if

    ierr = nf90_get_var( self%ncid,              &
                         self%last_halo_cell_id, &
                         self%last_halo_cell(:) )
    if (ierr == NF90_NOERR) then
      allocate(last_halo_cell, source=self%last_halo_cell)
    end if

    !=================================================
    ! Read in information on the local GLOBAL variable
    !=================================================
    id = self%global_mesh_id
    attname = 'npanels'
    ierr = nf90_get_att( self%ncid, id, trim(attname), &
                         self%npanels )
    if (ierr == NF90_NOERR) npanels = self%npanels

    attname = 'rim_depth'
    ierr = nf90_get_att( self%ncid, id, trim(attname), &
                         self%rim_depth )
    if (ierr == NF90_NOERR) rim_depth = self%rim_depth

    attname = 'ncells'
    ierr = nf90_get_att( self%ncid, id, trim(attname), &
                         self%nmesh_faces_global )
    if (ierr == NF90_NOERR) then
      num_faces_global = self%nmesh_faces_global
    end if

    attname = 'constructor_inputs'
    ierr = nf90_get_att( self%ncid, id, trim(attname), &
                         self%constructor_inputs )
    if (ierr == NF90_NOERR) then
      constructor_inputs = self%constructor_inputs
    end if

    ierr = nf90_get_var( self%ncid, self%domain_extents_id, &
                         self%domain_extents(:,:) )
    if (ierr == NF90_NOERR) then
      domain_extents(:,:) = self%domain_extents(:,:)
    end if

    ierr = nf90_get_var( self%ncid, self%global_cells_id, &
                         self%cell_gid(:) )
    if (ierr == NF90_NOERR) then
      allocate( cell_gid, source=self%cell_gid )
    end if

    ierr = nf90_get_var( self%ncid, self%global_face_node_id, &
                         self%node_on_cell_gid(:,:) )
    if (ierr == NF90_NOERR) then
      allocate( node_on_cell_gid, source=self%node_on_cell_gid )
    end if

    ierr = nf90_get_var( self%ncid, self%global_face_edge_id, &
                         self%edge_on_cell_gid(:,:) )
    if (ierr == NF90_NOERR) then
      allocate( edge_on_cell_gid, source=self%edge_on_cell_gid )
    end if

  case ( GLOBAL_MESH_FLAG )

    ! Read in global mesh related variables to main mesh
    attname = 'npanels'
    ierr = nf90_get_att( self%ncid, self%mesh_id, trim(attname), &
                         self%npanels )
    npanels = self%npanels

    attname = 'rim_depth'
    ierr = nf90_get_att( self%ncid, self%mesh_id, trim(attname), &
                         self%rim_depth )
    rim_depth = self%rim_depth

    attname = 'constructor_inputs'
    ierr = nf90_get_att( self%ncid, self%mesh_id, trim(attname), &
                         self%constructor_inputs )
    constructor_inputs = self%constructor_inputs

    ierr = nf90_get_var( self%ncid, self%domain_extents_id, &
                         self%domain_extents(:,:) )
    if (ierr == NF90_NOERR) then
      domain_extents(:,:) = self%domain_extents(:,:)
    end if

  end select

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
!>                                      [n target cells per source x,
!>                                       n target cells per source y,
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
  integer(i_def),     intent(out), allocatable :: mesh_map(:,:,:)

  character(*), parameter :: routine = 'read_map'

  integer(i_def) :: mesh_map_id
  integer(i_def) :: source_cells_id
  integer(i_def) :: target_cells_per_source_cell_id
  integer(i_def) :: target_cells_per_source_x_id
  integer(i_def) :: target_cells_per_source_y_id
  integer(i_def) :: ierr

  integer(i_def) :: source_ncells
  integer(i_def) :: target_cells_per_source_x
  integer(i_def) :: target_cells_per_source_y
  integer(i_def) :: target_cells_per_source_cell

  character(nf90_max_name) :: var_name, dim_name
  character(str_long)      :: cmess

  ! 1.0 Get number of cells for source map
  !=====================================================
  dim_name = 'n'//trim(source_mesh_name)//'_map_face'

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
             '_x'

  cmess = 'Getting '//trim(dim_name)//' id'
  ierr = nf90_inq_dimid( self%ncid, &
                         dim_name,  &
                         target_cells_per_source_x_id )
  call check_err(ierr, routine, cmess)

  cmess = 'Getting '//trim(dim_name)//' value'
  ierr = nf90_inquire_dimension( self%ncid,                    &
                                 target_cells_per_source_x_id, &
                                 len=target_cells_per_source_x )
  call check_err(ierr, routine, cmess)

  dim_name = 'n'//trim(target_mesh_name)//           &
             '_cells_per_'//trim(source_mesh_name)// &
             '_y'

  cmess = 'Getting '//trim(dim_name)//' id'
  ierr = nf90_inq_dimid( self%ncid, &
                         dim_name,  &
                         target_cells_per_source_y_id )
  call check_err(ierr, routine, cmess)

  cmess = 'Getting '//trim(dim_name)//' value'
  ierr = nf90_inquire_dimension( self%ncid,                    &
                                 target_cells_per_source_y_id, &
                                 len=target_cells_per_source_y )
  call check_err(ierr, routine, cmess)

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
  allocate( mesh_map( target_cells_per_source_x, &
                      target_cells_per_source_y, &
                      source_ncells )            )

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
                       mesh_map(:,:,:) )
  call check_err(ierr, routine, cmess)

end subroutine read_map


!-------------------------------------------------------------------------------
!> @brief   Adds mesh data to a new NetCDF file.
!> @details Adds mesh data to NetCDF UGRID mesh file:
!>          * dimension
!>          * coordinates
!>          * connectivity information
!>          * partition information
!>          * mesh metadata
!>
!> @param[in] mesh_name                Name of the mesh topology.
!> @param[in] geometry                 Mesh domain geometry.
!> @param[in] coord_sys                Co-ordinate system used to convey
!>                                     node locations.
!> @param[in] north_pole               [Longitude, Latitude] of north pole
!>                                     for domain orientation.
!> @param[in] null_island              [Longitude, Latitude] of null
!>                                     island for domain orientation.
!> @param[in] equatorial_latitude      Latitude of equator of mesh.
!> @param[in] num_nodes                The number of nodes on the mesh.
!> @param[in] num_edges                The number of edges on the mesh.
!> @param[in] num_faces                The number of faces on the mesh.
!> @param[in] node_coordinates         Coordinates of each node.
!> @param[in] face_coordinates         Coordinates of each face.
!> @param[in] coord_units_x            Units of x coordinates.
!> @param[in] coord_units_y            Units of y coordinates.
!> @param[in] void_cell                Values to mark a cell as external
!>                                     to domain.
!> @param[in] face_node_connectivity   Nodes adjoining each face.
!> @param[in] face_edge_connectivity   Edges adjoining each face.
!> @param[in] face_face_connectivity   Faces adjoining each face (links).
!> @param[in] edge_node_connectivity   Nodes adjoining each edge.
!> @param[in] topology                 Indicates layout of mesh connectivity
!> @param[in] periodic_xy              Periodic in x/y-axes.
!> @param[in] domain_extents           Principal coordinates that
!>                                     describe the domain shape.
!> @param[in] npanels                  Number of panels in this mesh
!> @param[in] rim_depth                Depth in cells of global mesh rim (LBC meshes).
!> @param[in] constructor_inputs       Inputs used to create this mesh
!>                                     from the ugrid_generator
!> @param[in] partition_of             Name of mesh that (local mesh) is a partition of.
!> @param[in] num_faces_global         Number of cells in the global mesh given by `partition_of`
!> @param[in] max_stencil_depth        Maximum stencil supported by this local mesh.
!> @param[in] inner_depth              Depth (in cells) of inner halos inward from edge cell layer.
!> @param[in] num_inner                Number of cells at each inner halo depth.
!> @param[in] last_inner_cell          Local id of the last cell at each inner halo depth.
!> @param[in] halo_depth               Depth (in cells) of halos outward from edge cell layer.
!> @param[in] num_halo                 Number of cells at each halo depth.
!> @param[in] last_halo_cell           Local id of the last cell at each halo depth.
!> @param[in] num_edge                 Number of cells in the partition edge cell layer.
!> @param[in] last_edge_cell           Local id of the last cell in the edge cell layer.
!> @param[in] num_ghost                Number of cells in the ghost cell layer.
!> @param[in] last_ghost_cell          Local id of the last cell in the ghost cell layer.
!> @param[in] node_cell_owner          Cell that "owns" a given node.
!> @param[in] edge_cell_owner          Cell that "owns" a given edge.
!> @param[in] cell_gid                 Global ids of local mesh cells.
!>                                     (partition, halos and ghost cells).
!> @param[in] node_on_cell_gid         Local node on cell connectivities listed with
!>                                     the node global IDs.
!> @param[in] edge_on_cell_gid         Local edge on cell connectivities listed with
!>                                     the edge global IDs.
!> @param[in] num_targets              Number of mesh maps from mesh.
!> @param[in] target_mesh_names        Mesh(es) that this mesh has maps for.
!> @param[in] target_global_mesh_maps  Mesh maps from this global mesh to target mesh(es).
!> @param[in] target_local_mesh_maps   Mesh maps from this local mesh to target mesh(es).
!-------------------------------------------------------------------------------
subroutine write_mesh( self,                                              &

                       ! Common mesh type variables.
                       mesh_name, geometry, coord_sys,                    &
                       north_pole, null_island, equatorial_latitude,      &
                       num_nodes, num_edges, num_faces,                   &
                       node_coordinates, face_coordinates,                &
                       coord_units_x, coord_units_y, void_cell,           &
                       face_node_connectivity, face_edge_connectivity,    &
                       face_face_connectivity, edge_node_connectivity,    &

                       ! Global mesh only variables.
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

  implicit none

  ! Arguments.
  class(ncdf_quad_type),  intent(inout) :: self

  ! Common mesh variables.
  character(str_def),  intent(in) :: mesh_name
  character(str_def),  intent(in) :: geometry
  character(str_def),  intent(in) :: coord_sys
  real(r_def),         intent(in) :: north_pole(2)
  real(r_def),         intent(in) :: null_island(2)
  real(r_def),         intent(in) :: equatorial_latitude
  integer(i_def),      intent(in) :: num_nodes
  integer(i_def),      intent(in) :: num_edges
  integer(i_def),      intent(in) :: num_faces
  real(r_def),         intent(in) :: node_coordinates(:,:)
  real(r_def),         intent(in) :: face_coordinates(:,:)
  character(str_def),  intent(in) :: coord_units_x
  character(str_def),  intent(in) :: coord_units_y
  integer(i_def),      intent(in) :: void_cell
  integer(i_def),      intent(in) :: face_node_connectivity(:,:)
  integer(i_def),      intent(in) :: face_edge_connectivity(:,:)
  integer(i_def),      intent(in) :: face_face_connectivity(:,:)
  integer(i_def),      intent(in) :: edge_node_connectivity(:,:)

  ! Global mesh variables.
  character(str_def),  intent(in) :: topology
  logical(l_def),      intent(in) :: periodic_xy(2)
  real(r_def),         intent(in) :: domain_extents(2,4)
  integer(i_def),      intent(in) :: npanels
  integer(i_def),      intent(in) :: rim_depth

  character(str_longlong), intent(in) :: constructor_inputs

  ! Partition variables.
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


  ! Internal variables
  character(*), parameter :: routine = 'write_mesh'
  character(str_long)     :: cmess
  integer(i_def)          :: ierr, i, ratio_x, ratio_y, cell
  integer(i_def)          :: cells_in_layer

  integer(i_def), allocatable :: cell_map(:,:,:), tmp_cell_map(:,:,:)

  type(global_mesh_map_type), pointer :: global_mesh_map => null()
  type(local_mesh_map_type),  pointer :: local_mesh_map  => null()

  ! We need to ensure that netcdf receives data with the appropriate
  ! precision to create temporary arrays to hold real data
  ! converted from/to default precision

  real(r_ncdf), allocatable :: node_coordinates_ncdf(:,:)
  real(r_ncdf), allocatable :: face_coordinates_ncdf(:,:)
  real(r_ncdf)              :: domain_extents_ncdf(2,4)

  integer(i_def) :: lower1,upper1,lower2,upper2

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

  node_coordinates_ncdf(:,:) =  real( node_coordinates(:,:), kind=r_ncdf )
  face_coordinates_ncdf(:,:) =  real( face_coordinates(:,:), kind=r_ncdf )

  self%geometry  = geometry
  self%topology  = topology
  self%coord_sys = coord_sys
  self%npanels   = npanels

  ! Determine if the contents of object is a global/regional model.
  if ( ( trim(self%geometry)  == 'spherical' ) .and. &
       ( trim(self%coord_sys) == 'll'        ) .and. &
       ( trim(self%topology)  == 'periodic'  ) .and. &
       ( self%npanels > 1 ) ) then
    self%model_extents  = GLOBAL_MODEL_FLAG
  else
    self%model_extents  = REGION_MODEL_FLAG
  end if

  self%domain_extents = domain_extents
  domain_extents_ncdf(:,:) = real( self%domain_extents(:,:), kind=r_ncdf )

  ! Determine if the contents of object is a global/local mesh.
  self%partition_of = partition_of
  if ( trim(self%partition_of) /= trim(cmdi) ) then
    self%mesh_extents = LOCAL_MESH_FLAG
  else
    self%mesh_extents = GLOBAL_MESH_FLAG
  end if

  self%mesh_name           = mesh_name
  self%north_pole(:)       = north_pole(:)
  self%null_island(:)      = null_island(:)
  self%equatorial_latitude = equatorial_latitude
  self%periodic_xy(:)      = periodic_xy(:)
  self%coord_units_x       = coord_units_x
  self%coord_units_y       = coord_units_y
  self%void_cell           = void_cell

  self%constructor_inputs = constructor_inputs

  self%nmesh_nodes = num_nodes
  self%nmesh_edges = num_edges
  self%nmesh_faces = num_faces
  self%rim_depth   = rim_depth

  ! Partition info
  self%nmesh_faces_global = num_faces_global
  self%inner_depth        = inner_depth
  self%halo_depth         = halo_depth
  self%num_edge           = num_edge
  self%last_edge_cell     = last_edge_cell
  self%num_ghost          = num_ghost
  self%last_ghost_cell    = last_ghost_cell
  self%max_stencil_depth  = max_stencil_depth

  ! InterGrid maps
  self%nmesh_targets = num_targets

  if ( self%nmesh_targets > 0 ) then

    if ( allocated(self%ntargets_per_source_dim_id)   ) deallocate( self%ntargets_per_source_dim_id   )
    if ( allocated(self%ntargets_per_source_x_dim_id) ) deallocate( self%ntargets_per_source_x_dim_id )
    if ( allocated(self%ntargets_per_source_y_dim_id) ) deallocate( self%ntargets_per_source_y_dim_id )

    if ( allocated(self%mesh_mesh_links_id) ) deallocate( self%mesh_mesh_links_id )
    if ( allocated(self%target_mesh_names)  ) deallocate( self%target_mesh_names )

    allocate( self%ntargets_per_source_dim_id   (self%nmesh_targets) )
    allocate( self%ntargets_per_source_x_dim_id (self%nmesh_targets) )
    allocate( self%ntargets_per_source_y_dim_id (self%nmesh_targets) )

    allocate( self%mesh_mesh_links_id (self%nmesh_targets) )
    allocate( self%target_mesh_names  (self%nmesh_targets) )

    do i=1, self%nmesh_targets
      self%target_mesh_names(i) = trim(target_mesh_names(i))
    end do

    select case (self%mesh_extents)
    case (GLOBAL_MESH_FLAG)
      self%target_global_mesh_maps => target_global_mesh_maps
    case (LOCAL_MESH_FLAG)
      self%target_local_mesh_maps  => target_local_mesh_maps
    end select

  end if ! nmesh_targets


  ! Set up NetCDF header
  call define_dimensions (self)
  call define_variables  (self)
  call assign_attributes (self)


  ! End definitions before putting data in.
  cmess = 'Closing netCDF definitions'
  ierr = nf90_enddef(self%ncid)
  call check_err(ierr, routine, cmess)


  ! Node coordinates.
  cmess = 'Writing node x-coords for mesh, "'//trim(mesh_name)//'"'
  ierr = nf90_put_var( self%ncid, self%mesh_node_x_id, node_coordinates_ncdf(1,:) )
  call check_err(ierr, routine, cmess)

  cmess = 'Writing node y-coords for mesh, "'//trim(mesh_name)//'"'
  ierr = nf90_put_var( self%ncid, self%mesh_node_y_id, node_coordinates_ncdf(2,:) )
  call check_err(ierr, routine, cmess)

  ! Face coordinates.
  cmess = 'Writing face x-coords for mesh, "'//trim(mesh_name)//'"'
  ierr = nf90_put_var( self%ncid, self%mesh_face_x_id, face_coordinates_ncdf(1,:) )
  call check_err(ierr, routine, cmess)

  cmess = 'Writing face y-coords for mesh, "'//trim(mesh_name)//'"'
  ierr = nf90_put_var( self%ncid, self%mesh_face_y_id, face_coordinates_ncdf(2,:) )
  call check_err(ierr, routine, cmess)

  ! Face-Node connectivity.
  cmess = 'Writing face-node connectivity for mesh, "'//trim(mesh_name)//'"'
  ierr = nf90_put_var( self%ncid, self%mesh_face_nodes_id, &
                       face_node_connectivity(:,:) )
  call check_err(ierr, routine, cmess)

  ! Face-Edge connectivity.
  cmess = 'Writing face-edge connectivity for mesh, "'//trim(mesh_name)//'"'
  ierr = nf90_put_var( self%ncid, self%mesh_face_edges_id, &
                       face_edge_connectivity(:,:) )
  call check_err(ierr, routine, cmess)

  ! Face-Face connectivity.
  cmess = 'Writing face-face connectivity for mesh, "'//trim(mesh_name)//'"'
  ierr = nf90_put_var( self%ncid, self%mesh_face_links_id, &
                       face_face_connectivity(:,:) )
  call check_err(ierr, routine, cmess)

  ! Edge-Node connectivity.
  cmess = 'Writing edge-node connectivity for mesh, "'//trim(mesh_name)//'"'
  ierr = nf90_put_var( self%ncid, self%mesh_edge_nodes_id, &
                       edge_node_connectivity(:,:) )
  call check_err(ierr, routine, cmess)


  ! Write partition/global info for local meshes.
  if (self%mesh_extents == LOCAL_MESH_FLAG) then

    ! Cell owners of nodes.
    cmess = 'Writing partition '// trim(self%mesh_name) // ' cell ownership of nodes.'
    ierr = nf90_put_var( self%ncid, self%node_cell_owner_id, node_cell_owner(:) )
    call check_err(ierr, routine, cmess)

    ! Cell owners of edges.
    cmess = 'Writing partition '// trim(self%mesh_name) // ' cell ownership of edges.'
    ierr = nf90_put_var( self%ncid, self%edge_cell_owner_id, edge_cell_owner(:) )
    call check_err(ierr, routine, cmess)

    ! Inner layers.
    if (self%inner_depth > 0) then
      cmess = 'Writing partition '// trim(self%mesh_name) // ' num inner.'
      ierr = nf90_put_var( self%ncid, self%num_inner_id, num_inner(:) )
      call check_err(ierr, routine, cmess)

      cmess = 'Writing partition '// trim(self%mesh_name) // ' last inner cell.'
      ierr = nf90_put_var( self%ncid, self%last_inner_cell_id, last_inner_cell(:) )
      call check_err(ierr, routine, cmess)
    end if

    ! Halo layers.
    if (self%halo_depth > 0) then
      cmess = 'Writing partition '// trim(self%mesh_name) // ' num halo.'
      ierr = nf90_put_var( self%ncid, self%num_halo_id, num_halo(:) )
      call check_err(ierr, routine, cmess)

      cmess = 'Writing partition '// trim(self%mesh_name) // ' last halo cell.'
      ierr = nf90_put_var( self%ncid, self%last_halo_cell_id, last_halo_cell(:) )
      call check_err(ierr, routine, cmess)
    end if

    ! Global cell ids on this partition (including those of ghost cells).
    cmess = 'Writing global mesh '// trim(self%mesh_name) // ' cell ids.'
    ierr = nf90_put_var( self%ncid, self%global_cells_id, cell_gid(:) )
    call check_err(ierr, routine, cmess)

    ! Local node on cell connectivity (Node IDs are as GIDs).
    cmess = 'Writing global mesh '// trim(self%mesh_name) // ' node on cell ids.'
    ierr = nf90_put_var( self%ncid, self%global_face_node_id, node_on_cell_gid(:,:) )
    call check_err(ierr, routine, cmess)

    ! Local edge on cell connectivity (Eode IDs are as GIDs).
    cmess = 'Writing global mesh '// trim(self%mesh_name) // ' edge on cell ids.'
    ierr = nf90_put_var( self%ncid, self%global_face_edge_id, edge_on_cell_gid(:,:) )
    call check_err(ierr, routine, cmess)

  end if

  ! Global mesh domain extents.
  cmess = 'Writing global mesh '// trim(self%mesh_name) // ' domain extents.'
  ierr = nf90_put_var( self%ncid, self%domain_extents_id, domain_extents_ncdf(:,:) )
  call check_err(ierr, routine, cmess)


  ! Mesh_Mesh connectivity (Inter-grid maps)
  select case (self%mesh_extents)

  case (GLOBAL_MESH_FLAG)

    do i=1, num_targets
      nullify(global_mesh_map)
      global_mesh_map => self%target_global_mesh_maps%get_global_mesh_map(1,i+1)
      ratio_x = global_mesh_map%get_ntarget_cells_per_source_x()
      ratio_y = global_mesh_map%get_ntarget_cells_per_source_y()
      allocate(cell_map(ratio_x, ratio_y, self%nmesh_faces))
      allocate(tmp_cell_map(ratio_x, ratio_y, 1))
      do cell=1, self%nmesh_faces
        call global_mesh_map%get_cell_map([cell], tmp_cell_map)
        cell_map(:,:,cell) = tmp_cell_map(:,:,1)
      end do
      cmess = 'Writing mesh-mesh connectivity for meshes, "'// &
              trim(self%mesh_name)//'"->"'//                   &
              trim(self%target_mesh_names(i))//'"'
      ierr = nf90_put_var( self%ncid, self%mesh_mesh_links_id(i), cell_map(:,:,:) )
      deallocate(cell_map)
      deallocate(tmp_cell_map)
      call check_err(ierr, routine, cmess)
    end do

  case (LOCAL_MESH_FLAG)

    ! Local mesh will only have mesh maps up to the halo cells, i.e.
    ! ghost cells will not have maps
    cells_in_layer = self%nmesh_faces - self%num_ghost

    do i=1, num_targets
      nullify(local_mesh_map)
      local_mesh_map => self%target_local_mesh_maps%get_local_mesh_map(1,i+1)
      ratio_x = local_mesh_map%get_ntarget_cells_per_source_cell_x()
      ratio_y = local_mesh_map%get_ntarget_cells_per_source_cell_y()
      allocate(cell_map(ratio_x, ratio_y, cells_in_layer))
      allocate(tmp_cell_map(ratio_x, ratio_y, 1))
      do cell=1, cells_in_layer
        call local_mesh_map%get_cell_map([cell], tmp_cell_map)
        cell_map(:,:,cell) = tmp_cell_map(:,:,1)
      end do
      cmess = 'Writing mesh-mesh connectivity for meshes, "'// &
              trim(self%mesh_name)//'"->"'//                   &
              trim(self%target_mesh_names(i))//'"'
      ierr = nf90_put_var( self%ncid, self%mesh_mesh_links_id(i), cell_map(:,:,:) )
      deallocate(cell_map)
      deallocate(tmp_cell_map)
      call check_err(ierr, routine, cmess)
    end do

  end select

  deallocate(node_coordinates_ncdf)
  deallocate(face_coordinates_ncdf)

  return
end subroutine write_mesh


!-------------------------------------------------------------------------------
!>  @brief Function to determine if mesh the NetCDF UGRID file is a global or
!>         local mesh.
!>
!>  @param[in] mesh_name  Name of the mesh topology
!>  @return    answer     .True. if mesh in is a local mesh.
!-------------------------------------------------------------------------------
function is_mesh_local(self, mesh_name) result (answer)

  implicit none

  ! Arguments
  type(ncdf_quad_type), intent(inout) :: self

  character(str_def), intent(in) :: mesh_name

  logical(l_def) :: answer

  ! Internal variables
  character(*), parameter :: routine = 'is_mesh_local'

  character(str_long) :: cmess
  integer(i_def)      :: ierr
  logical(l_def)      :: mesh_present

  mesh_present = self%is_mesh_present(mesh_name)

  if (.not. mesh_present) then
    write( log_scratch_space,'(A)' ) &
        'Mesh '//trim(mesh_name)//' not present in file.'
    call log_event(trim(log_scratch_space), LOG_LEVEL_ERROR)
  end if

  cmess = 'Getting mesh netcdf id for"'//trim(mesh_name)//'"'
  ierr  = nf90_inq_varid( self%ncid, trim(mesh_name), self%mesh_id )
  call check_err(ierr, routine, cmess)

  ! Mesh partition info
  ierr = nf90_get_att( self%ncid, self%mesh_id, &
                       'partition_info', self%partition_of )

  if ( ierr == nf90_noerr ) then
    answer = .true.
  else
    answer = .false.
  end if

end function is_mesh_local


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
!> @brief Adds a mesh to an existing NetCDF ugrid file
!> @details Adds mesh data to NetCDF UGRID mesh file:
!>          * dimension
!>          * coordinates
!>          * connectivity information
!>          * partition information
!>          * mesh metadata
!>
!> @param[in] mesh_name                Name of the mesh topology.
!> @param[in] geometry                 Mesh domain geometry.
!> @param[in] coord_sys                Co-ordinate system used to convey
!>                                     node locations.
!> @param[in] north_pole               [Longitude, Latitude] of north pole
!>                                     for domain orientation
!> @param[in] null_island              [Longitude, Latitude] of null
!>                                     island for domain orientation
!> @param[in] equatorial_latitude      Latitude of equator of mesh.
!> @param[in] num_nodes                The number of nodes on the mesh.
!> @param[in] num_edges                The number of edges on the mesh.
!> @param[in] num_faces                The number of faces on the mesh.
!> @param[in] node_coordinates         Coordinates of each node.
!> @param[in] face_coordinates         Coordinates of each face.
!> @param[in] coord_units_x            Units of x coordinates.
!> @param[in] coord_units_y            Units of y coordinates.
!> @param[in] void_cell                Values to mark a cell as external
!>                                     to domain.
!> @param[in] face_node_connectivity   Nodes adjoining each face.
!> @param[in] face_edge_connectivity   Edges adjoining each face.
!> @param[in] face_face_connectivity   Faces adjoining each face (links).
!> @param[in] edge_node_connectivity   Nodes adjoining each edge.
!> @param[in] topology                 Indicates layout of mesh connectivity
!> @param[in] periodic_xy              Periodic in x/y-axes.
!> @param[in] domain_extents           Coordinates of principal nodes that
!>                                     describe the domain shape.
!> @param[in] npanels                  Number of panels in this mesh
!> @param[in] rim_depth                Depth in cells of global mesh rim (LBC meshes).
!> @param[in] constructor_inputs       Inputs used to create this mesh
!>                                     from the ugrid_generator
!> @param[in] partition_of             Name of mesh that (local mesh) is a partition of.
!> @param[in] num_faces_global         Number of cells in the global mesh given by `partition_of`
!> @param[in] max_stencil_depth        Maximum stencil supported by this local mesh.
!> @param[in] inner_depth              Depth (in cells) of inner halos inward from edge cell layer.
!> @param[in] num_inner                Number of cells at each inner halo depth.
!> @param[in] last_inner_cell          Local id of the last cell at each inner halo depth.
!> @param[in] halo_depth               Depth (in cells) of halos outward from edge cell layer.
!> @param[in] num_halo                 Number of cells at each halo depth.
!> @param[in] last_halo_cell           Local id of the last cell at each halo depth.
!> @param[in] num_edge                 Number of cells in the partition edge cell layer.
!> @param[in] last_edge_cell           Local id of the last cell in the edge cell layer.
!> @param[in] num_ghost                Number of cells in the ghost cell layer.
!> @param[in] last_ghost_cell          Local id of the last cell in the ghost cell layer.
!> @param[in] node_cell_owner          Cell that "owns" a given node.
!> @param[in] edge_cell_owner          Cell that "owns" a given edge.
!> @param[in] cell_gid                 Global ids of local mesh cells.
!>                                     (partition, halos and ghost cells).
!> @param[in] node_on_cell_gid         Local node on cell connectivities listed with
!>                                     the node global IDs.
!> @param[in] edge_on_cell_gid         Local edge on cell connectivities listed with
!>                                     the edge global IDs.
!> @param[in] num_targets              Number of mesh maps from mesh.
!> @param[in] target_mesh_names        Mesh(es) that this mesh has maps for.
!> @param[in] target_global_mesh_maps  Mesh maps from this global mesh to target mesh(es).
!> @param[in] target_local_mesh_maps   Mesh maps from this local mesh to target mesh(es).
!-------------------------------------------------------------------------------
subroutine append_mesh( self,                                              &

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

                        ! Inter-grid maps.
                        num_targets, target_mesh_names,                    &
                        target_global_mesh_maps, target_local_mesh_maps )

  implicit none

  ! Arguments
  class(ncdf_quad_type),  intent(inout) :: self

  ! Common mesh variables.
  character(str_def),  intent(in) :: mesh_name
  character(str_def),  intent(in) :: geometry
  character(str_def),  intent(in) :: coord_sys
  real(r_def),         intent(in) :: north_pole(2)
  real(r_def),         intent(in) :: null_island(2)
  real(r_def),         intent(in) :: equatorial_latitude

  integer(i_def),      intent(in) :: num_nodes
  integer(i_def),      intent(in) :: num_edges
  integer(i_def),      intent(in) :: num_faces
  real(r_def),         intent(in) :: node_coordinates(:,:)
  real(r_def),         intent(in) :: face_coordinates(:,:)
  character(str_def),  intent(in) :: coord_units_x
  character(str_def),  intent(in) :: coord_units_y
  integer(i_def),      intent(in) :: void_cell
  integer(i_def),      intent(in) :: face_node_connectivity(:,:)
  integer(i_def),      intent(in) :: face_edge_connectivity(:,:)
  integer(i_def),      intent(in) :: face_face_connectivity(:,:)
  integer(i_def),      intent(in) :: edge_node_connectivity(:,:)

  ! Global mesh only variables
  character(str_def),  intent(in) :: topology
  logical(l_def),      intent(in) :: periodic_xy(2)
  real(r_def),         intent(in) :: domain_extents(2,4)
  integer(i_def),      intent(in) :: npanels
  integer(i_def),      intent(in) :: rim_depth

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
                      intent(in), pointer     :: target_global_mesh_maps
  type(local_mesh_map_collection_type),  &
                      intent(in), pointer     :: target_local_mesh_maps


  ! Internal variables
  character(*), parameter :: routine = 'append_mesh'
  integer(i_def)          :: ierr
  character(str_long)     :: cmess
  logical(l_def)          :: mesh_present

  mesh_present = self%is_mesh_present(mesh_name)

  if (mesh_present) then
    write(log_scratch_space,'(A)') &
        'Mesh '//trim(mesh_name)//' already used or is not unique.'
    call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    return
  end if

  cmess = 'Opening definitions for netCDF file'
  ierr = nf90_redef(self%ncid)
  call check_err(ierr, routine, cmess)

  call self%write_mesh(                                 &
       ! Common variables
       mesh_name              = mesh_name,              &
       geometry               = geometry,               &
       coord_sys              = coord_sys,              &
       north_pole             = north_pole,             &
       null_island            = null_island,            &
       equatorial_latitude    = equatorial_latitude,    &
       num_nodes              = num_nodes,              &
       num_edges              = num_edges,              &
       num_faces              = num_faces,              &
       node_coordinates       = node_coordinates,       &
       face_coordinates       = face_coordinates,       &
       coord_units_x          = coord_units_x,          &
       coord_units_y          = coord_units_y,          &
       void_cell              = void_cell,              &
       face_node_connectivity = face_node_connectivity, &
       face_edge_connectivity = face_edge_connectivity, &
       face_face_connectivity = face_face_connectivity, &
       edge_node_connectivity = edge_node_connectivity, &

       ! Global mesh info
       topology           = topology,           &
       periodic_xy        = periodic_xy,        &
       domain_extents     = domain_extents,     &
       npanels            = npanels,            &
       rim_depth          = rim_depth,          &
       constructor_inputs = constructor_inputs, &

       ! Partition info
       partition_of       = partition_of,       &
       num_faces_global   = num_faces_global,   &
       max_stencil_depth  = max_stencil_depth,  &
       inner_depth        = inner_depth,        &
       num_inner          = num_inner,          &
       last_inner_cell    = last_inner_cell,    &
       halo_depth         = halo_depth,         &
       num_halo           = num_halo,           &
       last_halo_cell     = last_halo_cell,     &
       num_edge           = num_edge,           &
       last_edge_cell     = last_edge_cell,     &
       num_ghost          = num_ghost,          &
       last_ghost_cell    = last_ghost_cell,    &
       node_cell_owner    = node_cell_owner,    &
       edge_cell_owner    = edge_cell_owner,    &
       cell_gid           = cell_gid,           &
       node_on_cell_gid   = node_on_cell_gid,   &
       edge_on_cell_gid   = edge_on_cell_gid,   &

       ! Intergrid maps
       num_targets             = num_targets,             &
       target_mesh_names       = target_mesh_names,       &
       target_global_mesh_maps = target_global_mesh_maps, &
       target_local_mesh_maps  = target_local_mesh_maps )

  return
end subroutine append_mesh

!-------------------------------------------------------------------------------
!>  @brief   Returns the NetCDF variable names in the NetCDF file which are
!>           ugrid mesh topologies.
!>  @details Scans the variable attributes for cf_role = mesh_topology as
!>           a flag that the variable name relates to a mesh.
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

  character(*), parameter :: routine = 'scan_for_topologies'
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
