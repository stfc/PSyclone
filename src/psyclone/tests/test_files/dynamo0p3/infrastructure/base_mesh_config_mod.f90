!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the base_mesh namelist.
!>
module base_mesh_config_mod

  use constants_mod, only: i_native, &
                           l_def, &
                           r_def, &
                           str_def, &
                           str_max_filename
  use log_mod,       only: log_event, log_scratch_space &
                         , LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, LOG_LEVEL_INFO
  use mpi_mod,       only: broadcast
  !use mpi,           only: MPI_SUCCESS

  use constants_mod, only: cmdi, emdi, imdi, PI, rmdi, unset_key

  implicit none

  private
  public :: geometry_from_key, key_from_geometry, &
            topology_from_key, key_from_topology, &
            read_base_mesh_namelist, postprocess_base_mesh_namelist, &
            base_mesh_is_loadable, base_mesh_is_loaded, base_mesh_final

  integer(i_native), public, parameter :: geometry_planar = 796
  integer(i_native), public, parameter :: geometry_spherical = 411
  integer(i_native), public, parameter :: topology_fully_periodic = 965
  integer(i_native), public, parameter :: topology_non_periodic = 842

  real(r_def), public, protected :: f_lat = rmdi
  real(r_def), public, protected :: f_lat_deg = rmdi
  character(str_max_filename), public, protected :: filename = cmdi
  logical(l_def), public, protected :: fplane = .false.
  integer(i_native), public, protected :: geometry = emdi
  logical(l_def), public, protected :: offline_partitioning = .false.
  character(str_def), public, protected :: prime_mesh_name = cmdi
  integer(i_native), public, protected :: topology = emdi

  logical :: namelist_loaded = .false.

  character(str_def), parameter :: geometry_key(2) &
          = [character(len=str_def) :: 'planar', &
                                       'spherical']
  character(str_def), parameter :: topology_key(2) &
          = [character(len=str_def) :: 'fully_periodic', &
                                       'non_periodic']

  integer(i_native), parameter :: geometry_value(2) &
          = [796_i_native, &
             411_i_native]
  integer(i_native), parameter :: topology_value(2) &
          = [965_i_native, &
             842_i_native]

contains

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  integer(i_native) function geometry_from_key( key )

    implicit none

    character(*), intent(in) :: key

    integer(i_native) :: key_index

    if (key == unset_key) then
      write( log_scratch_space, '(A)') &
          'Missing key for geometry enumeration in base_mesh namelist.'
      geometry_from_key = emdi
      call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      return
    end if

    key_index = 1
    do
      if (trim(geometry_key(key_index)) == trim(key)) then
        geometry_from_key = geometry_value(key_index)
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(geometry_key, 1)) then
          write( log_scratch_space, &
              '("Key ''", A, "'' not recognised for base_mesh geometry")' ) &
              trim(adjustl(key))
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function geometry_from_key

  !> Gets the enumeration key corresponding to a particular value.
  !>
  !> An error is reported if the value is not within range.
  !>
  !> @param[in] value Enumeration value.
  !>
  character(str_def) function key_from_geometry( value )

    implicit none

    integer(i_native), intent(in) :: value

    integer(i_native) :: value_index

    value_index = 1
    do
      if (geometry_value(value_index) == emdi) then
        key_from_geometry = unset_key
        return
      else if (geometry_value(value_index) == value) then
        key_from_geometry = geometry_key(value_index)
        return
      else
        value_index = value_index + 1
        if (value_index > ubound(geometry_key, 1)) then
          write( log_scratch_space, &
                 '("Value ", I0, " is not in base_mesh geometry")' ) value
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function key_from_geometry

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  integer(i_native) function topology_from_key( key )

    implicit none

    character(*), intent(in) :: key

    integer(i_native) :: key_index

    if (key == unset_key) then
      write( log_scratch_space, '(A)') &
          'Missing key for topology enumeration in base_mesh namelist.'
      topology_from_key = emdi
      call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      return
    end if

    key_index = 1
    do
      if (trim(topology_key(key_index)) == trim(key)) then
        topology_from_key = topology_value(key_index)
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(topology_key, 1)) then
          write( log_scratch_space, &
              '("Key ''", A, "'' not recognised for base_mesh topology")' ) &
              trim(adjustl(key))
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function topology_from_key

  !> Gets the enumeration key corresponding to a particular value.
  !>
  !> An error is reported if the value is not within range.
  !>
  !> @param[in] value Enumeration value.
  !>
  character(str_def) function key_from_topology( value )

    implicit none

    integer(i_native), intent(in) :: value

    integer(i_native) :: value_index

    value_index = 1
    do
      if (topology_value(value_index) == emdi) then
        key_from_topology = unset_key
        return
      else if (topology_value(value_index) == value) then
        key_from_topology = topology_key(value_index)
        return
      else
        value_index = value_index + 1
        if (value_index > ubound(topology_key, 1)) then
          write( log_scratch_space, &
                 '("Value ", I0, " is not in base_mesh topology")' ) value
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function key_from_topology

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] local_rank Rank of current process.
  !>
  subroutine read_base_mesh_namelist( file_unit, local_rank )

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank

    call read_namelist( file_unit, local_rank, &
                        geometry, &
                        topology )

  end subroutine read_base_mesh_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, local_rank, &
                            dummy_geometry, &
                            dummy_topology )

    use constants_mod, only: i_def, r_def

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank
    integer(i_def)                :: missing_data
    integer(i_native), intent(out) :: dummy_geometry
    integer(i_native), intent(out) :: dummy_topology

    character(str_def) :: buffer_character_str_def(1)
    character(str_max_filename) :: buffer_character_str_max_filename(1)
    integer(i_native) :: buffer_integer_i_native(2)
    integer(i_native) :: buffer_logical_l_def(2)
    real(r_def) :: buffer_real_r_def(1)

    character(str_def) :: geometry
    character(str_def) :: topology

    namelist /base_mesh/ f_lat_deg, &
                         filename, &
                         fplane, &
                         geometry, &
                         offline_partitioning, &
                         prime_mesh_name, &
                         topology

    integer(i_native) :: condition

    missing_data = 0

    f_lat = rmdi
    f_lat_deg = rmdi
    filename = cmdi
    fplane = .false.
    geometry = unset_key
    offline_partitioning = .false.
    prime_mesh_name = cmdi
    topology = unset_key

    if (local_rank == 0) then

      read( file_unit, nml=base_mesh, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      dummy_geometry = geometry_from_key( geometry )
      dummy_topology = topology_from_key( topology )

    end if

    buffer_real_r_def(1) = f_lat_deg
    buffer_character_str_max_filename(1) = filename
    buffer_logical_l_def(1) = merge( 1, 0, fplane )
    buffer_integer_i_native(1) = dummy_geometry
    buffer_logical_l_def(2) = merge( 1, 0, offline_partitioning )
    buffer_character_str_def(1) = prime_mesh_name
    buffer_integer_i_native(2) = dummy_topology

    call broadcast( buffer_character_str_def, 1*str_def, 0 )
    call broadcast( buffer_character_str_max_filename, 1*str_max_filename, 0 )
    call broadcast( buffer_integer_i_native, 2, 0 )
    call broadcast( buffer_logical_l_def, 2, 0 )
    call broadcast( buffer_real_r_def, 1, 0 )

    f_lat_deg = buffer_real_r_def(1)
    filename = buffer_character_str_max_filename(1)
    fplane = buffer_logical_l_def(1) /= 0
    dummy_geometry = buffer_integer_i_native(1)
    offline_partitioning = buffer_logical_l_def(2) /= 0
    prime_mesh_name = buffer_character_str_def(1)
    dummy_topology = buffer_integer_i_native(2)

   ! Parameter name f_lat: dereferenced_list_vars are: ['f_lat_deg']
    missing_data = 0
    if (kind(f_lat_deg) == r_def) then
       if (real(f_lat_deg, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(f_lat_deg) == i_def) then
       if (int(f_lat_deg, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if ( missing_data >=1 ) then
       f_lat = rmdi
    else
       f_lat = f_lat_deg * PI / 180.0_r_def
    end if

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_base_mesh_namelist()

    implicit none


  end subroutine postprocess_base_mesh_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function base_mesh_is_loadable()

    implicit none

    logical :: base_mesh_is_loadable

    base_mesh_is_loadable = .not. namelist_loaded

  end function base_mesh_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function base_mesh_is_loaded()

    implicit none

    logical :: base_mesh_is_loaded

    base_mesh_is_loaded = namelist_loaded

  end function base_mesh_is_loaded

  !> Clear out any allocated memory
  !>
  subroutine base_mesh_final()

    implicit none

    f_lat = real(rmdi,r_def)
    f_lat_deg = real(rmdi,r_def)
    filename = cmdi
    fplane = .false.
    geometry = emdi
    offline_partitioning = .false.
    prime_mesh_name = cmdi
    topology = emdi

    return
  end subroutine base_mesh_final


end module base_mesh_config_mod
