!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2020, Science and Technology Facilities Council.
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
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

  implicit none

  private
  public :: geometry_from_key, key_from_geometry, &
            partitioner_from_key, key_from_partitioner, &
            read_base_mesh_namelist, postprocess_base_mesh_namelist, &
            base_mesh_is_loadable, base_mesh_is_loaded, base_mesh_final

  integer(i_native), public, parameter :: geometry_planar = 781
  integer(i_native), public, parameter :: geometry_spherical = 348
  integer(i_native), public, parameter :: partitioner_cubedsphere = 791
  integer(i_native), public, parameter :: partitioner_planar = 200

  real(r_def), public, protected :: f_lat
  real(r_def), public, protected :: f_lat_deg
  character(str_max_filename), public, protected :: filename
  logical(l_def), public, protected :: fplane
  integer(i_native), public, protected :: geometry
  integer(i_native), public, protected :: partitioner
  character(str_def), public, protected :: prime_mesh_name

  logical :: namelist_loaded = .false.

  character(str_def), parameter :: geometry_key(2) &
          = [character(len=str_def) :: 'planar', &
                                       'spherical']
  character(str_def), parameter :: partitioner_key(2) &
          = [character(len=str_def) :: 'cubedsphere', &
                                       'planar']

  integer(i_native), parameter :: geometry_value(2) &
          = [781_i_native, &
             348_i_native]
  integer(i_native), parameter :: partitioner_value(2) &
          = [791_i_native, &
             200_i_native]

contains

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  integer(i_native) function geometry_from_key( key )

    use constants_mod, only: imdi, unset_key

    implicit none

    character(*), intent(in) :: key

    integer(i_native) :: key_index

    if (key == unset_key) then
      write( log_scratch_space, '(A)') &
          'Missing key for geometry enumeration in base_mesh namelist.'
      geometry_from_key = int(imdi,i_native)
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
                 '("Key ''", A, "'' not recognised for base_mesh geometry")' ) key
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

    use constants_mod, only: imdi, unset_key

    implicit none

    integer(i_native), intent(in) :: value

    integer(i_native) :: value_index

    value_index = 1
    do
      if (geometry_value(value_index) == int(imdi,i_native)) then
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
  integer(i_native) function partitioner_from_key( key )

    use constants_mod, only: unset_key, imdi

    implicit none

    character(*), intent(in) :: key

    integer(i_native) :: key_index

    if (key == unset_key) then
      write( log_scratch_space, '(A)') &
          'Missing key for partitioner enumeration in base_mesh namelist.'
      partitioner_from_key = int(imdi,i_native)
      call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      return
    end if

    key_index = 1
    do
      if (trim(partitioner_key(key_index)) == trim(key)) then
        partitioner_from_key = partitioner_value(key_index)
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(partitioner_key, 1)) then
          write( log_scratch_space, &
                 '("Key ''", A, "'' not recognised for base_mesh partitioner")' ) key
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function partitioner_from_key

  !> Gets the enumeration key corresponding to a particular value.
  !>
  !> An error is reported if the value is not within range.
  !>
  !> @param[in] value Enumeration value.
  !>
  character(str_def) function key_from_partitioner( value )

    use constants_mod, only: unset_key, imdi

    implicit none

    integer(i_native), intent(in) :: value

    integer(i_native) :: value_index

    value_index = 1
    do
      if (partitioner_value(value_index) == int(imdi,i_native)) then
        key_from_partitioner = unset_key
        return
      else if (partitioner_value(value_index) == value) then
        key_from_partitioner = partitioner_key(value_index)
        return
      else
        value_index = value_index + 1
        if (value_index > ubound(partitioner_key, 1)) then
          write( log_scratch_space, &
                 '("Value ", I0, " is not in base_mesh partitioner")' ) value
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function key_from_partitioner

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
                        partitioner )

  end subroutine read_base_mesh_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, local_rank, &
                            dummy_geometry, &
                            dummy_partitioner )

    use constants_mod, only: cmdi, unset_key, imdi, PI, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank
    integer(i_native), intent(out) :: dummy_geometry
    integer(i_native), intent(out) :: dummy_partitioner

    character(str_def) :: buffer_character_str_def(1)
    character(str_max_filename) :: buffer_character_str_max_filename(1)
    integer(i_native) :: buffer_integer_i_native(2)
    integer(i_native) :: buffer_logical_l_def(1)
    real(r_def) :: buffer_real_r_def(1)

    character(str_def) :: geometry
    character(str_def) :: partitioner

    namelist /base_mesh/ f_lat_deg, &
                         filename, &
                         fplane, &
                         geometry, &
                         partitioner, &
                         prime_mesh_name

    integer(i_native) :: condition

    f_lat = rmdi
    f_lat_deg = rmdi
    filename = cmdi
    fplane = .false.
    geometry = unset_key
    partitioner = unset_key
    prime_mesh_name = cmdi

    if (local_rank == 0) then

      read( file_unit, nml=base_mesh, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      dummy_geometry = geometry_from_key( geometry )
      dummy_partitioner = partitioner_from_key( partitioner )

    end if

    buffer_real_r_def(1) = f_lat_deg
    buffer_character_str_max_filename(1) = filename
    buffer_logical_l_def(1) = merge( 1, 0, fplane )
    buffer_integer_i_native(1) = dummy_geometry
    buffer_integer_i_native(2) = dummy_partitioner
    buffer_character_str_def(1) = prime_mesh_name

    f_lat_deg = buffer_real_r_def(1)
    filename = buffer_character_str_max_filename(1)
    fplane = buffer_logical_l_def(1) /= 0
    dummy_geometry = buffer_integer_i_native(1)
    dummy_partitioner = buffer_integer_i_native(2)
    prime_mesh_name = buffer_character_str_def(1)


    if ( any([f_lat_deg] == imdi) .or. &
         any([f_lat_deg] == rmdi) ) then
      f_lat = rmdi
    else
      f_lat = f_lat_deg * PI / 180.0_r_def
    end if

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_base_mesh_namelist()

    use constants_mod, only: cmdi, emdi, imdi, PI, rmdi

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

    use constants_mod, only: cmdi, emdi, imdi, PI, rmdi

    implicit none

    f_lat = real(rmdi,r_def)
    f_lat_deg = real(rmdi,r_def)
    filename = cmdi
    fplane = .false.
    geometry = int(imdi,i_native)
    partitioner = int(imdi,i_native)
    prime_mesh_name = cmdi

    return
  end subroutine base_mesh_final


end module base_mesh_config_mod
