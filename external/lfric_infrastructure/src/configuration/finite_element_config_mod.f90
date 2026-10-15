!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Manages the finite_element namelist.
!>
module finite_element_config_mod

  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit

  use constants_mod, only: i_def, &
                           l_def, &
                           str_def
  use lfric_mpi_mod, only: global_mpi
  use log_mod,       only: log_event, log_scratch_space, log_level, &
                           LOG_LEVEL_ERROR, LOG_LEVEL_DEBUG, LOG_LEVEL_INFO

  use namelist_mod,      only: namelist_type
  use namelist_item_mod, only: namelist_item_type

  use constants_mod, only: cmdi, emdi, imdi, rmdi, str_def, unset_key

  implicit none

  private
  public :: cellshape_from_key, key_from_cellshape, &
            coord_space_from_key, key_from_coord_space, &
            coord_system_from_key, key_from_coord_system, &
            read_finite_element_namelist, postprocess_finite_element_namelist, &
            finite_element_is_loadable, finite_element_is_loaded, &
            finite_element_reset_load_status, &
            finite_element_multiples_allowed, finite_element_final, &
            get_finite_element_nml, get_new_finite_element_nml

  integer(i_def), public, parameter :: cellshape_quadrilateral = 105049454
  integer(i_def), public, parameter :: cellshape_triangle = 1767501692
  integer(i_def), public, parameter :: coord_space_W0 = 483276385
  integer(i_def), public, parameter :: coord_space_Wchi = 87419380
  integer(i_def), public, parameter :: coord_space_Wtheta = 1122186853
  integer(i_def), public, parameter :: coord_system_native = 409039244
  integer(i_def), public, parameter :: coord_system_xyz = 745175937

  integer(i_def), public, protected :: cellshape = emdi
  integer(i_def), public, protected :: coord_order = imdi
  integer(i_def), public, protected :: coord_order_nonprime = imdi
  integer(i_def), public, protected :: coord_space = emdi
  integer(i_def), public, protected :: coord_system = emdi
  integer(i_def), public, protected :: element_order_h = imdi
  integer(i_def), public, protected :: element_order_v = imdi
  integer(i_def), public, protected :: nqp_h_exact = imdi
  integer(i_def), public, protected :: nqp_v_exact = imdi
  logical(l_def), public, protected :: rehabilitate = .false.
  logical(l_def), public, protected :: vorticity_in_w1 = .false.

  character(*), parameter :: listname = 'finite_element'
  character(str_def) :: profile_name = cmdi

  logical, parameter :: multiples_allowed = .false.

  logical :: nml_loaded = .false.

  character(str_def), parameter :: cellshape_key(2) &
          = [character(len=str_def) :: 'quadrilateral', &
                                       'triangle']
  character(str_def), parameter :: coord_space_key(3) &
          = [character(len=str_def) :: 'W0', &
                                       'Wchi', &
                                       'Wtheta']
  character(str_def), parameter :: coord_system_key(2) &
          = [character(len=str_def) :: 'native', &
                                       'xyz']

  integer(i_def), parameter :: cellshape_value(2) &
          = [105049454_i_def, &
             1767501692_i_def]
  integer(i_def), parameter :: coord_space_value(3) &
          = [483276385_i_def, &
             87419380_i_def, &
             1122186853_i_def]
  integer(i_def), parameter :: coord_system_value(2) &
          = [409039244_i_def, &
             745175937_i_def]

contains

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  function cellshape_from_key( key ) result (enum_value)

    implicit none

    character(*), intent(in) :: key

    integer(i_def) :: enum_value
    integer(i_def) :: key_index

    enum_value = emdi

    if (key == unset_key) then
      if (log_level() >= LOG_LEVEL_DEBUG ) then
        write(output_unit, '(A)') &
            'Missing key for cellshape enumeration ' // &
            'in finite_element namelist.'
        flush(output_unit)
      end if
      return
    end if

    key_index = 1
    do
      if (trim(cellshape_key(key_index)) == trim(key)) then
        enum_value = cellshape_value(key_index)
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(cellshape_key, 1)) then
          write(error_unit, '(A)') &
              'Key ' // trim(adjustl(key)) // ' not recognised for ' // &
              'cellshape in finite_element namelist.'
          flush(error_unit)
          stop 1
        end if
      end if
    end do

  end function cellshape_from_key

  !> Gets the enumeration key corresponding to a particular value.
  !>
  !> An error is reported if the value is not within range.
  !>
  !> @param[in] value Enumeration value.
  !>
  character(str_def) function key_from_cellshape( value )

    implicit none

    integer(i_def), intent(in) :: value

    integer(i_def) :: value_index

    value_index = 1
    do
      if (cellshape_value(value_index) == emdi) then
        key_from_cellshape = unset_key
        return
      else if (cellshape_value(value_index) == value) then
        key_from_cellshape = cellshape_key(value_index)
        return
      else
        value_index = value_index + 1
        if (value_index > ubound(cellshape_key, 1)) then
          write( log_scratch_space, &
                 '("Value ", I0, " is not in finite_element cellshape")' ) value
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function key_from_cellshape

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  function coord_space_from_key( key ) result (enum_value)

    implicit none

    character(*), intent(in) :: key

    integer(i_def) :: enum_value
    integer(i_def) :: key_index

    enum_value = emdi

    if (key == unset_key) then
      if (log_level() >= LOG_LEVEL_DEBUG ) then
        write(output_unit, '(A)') &
            'Missing key for coord_space enumeration ' // &
            'in finite_element namelist.'
        flush(output_unit)
      end if
      return
    end if

    key_index = 1
    do
      if (trim(coord_space_key(key_index)) == trim(key)) then
        enum_value = coord_space_value(key_index)
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(coord_space_key, 1)) then
          write(error_unit, '(A)') &
              'Key ' // trim(adjustl(key)) // ' not recognised for ' // &
              'coord_space in finite_element namelist.'
          flush(error_unit)
          stop 1
        end if
      end if
    end do

  end function coord_space_from_key

  !> Gets the enumeration key corresponding to a particular value.
  !>
  !> An error is reported if the value is not within range.
  !>
  !> @param[in] value Enumeration value.
  !>
  character(str_def) function key_from_coord_space( value )

    implicit none

    integer(i_def), intent(in) :: value

    integer(i_def) :: value_index

    value_index = 1
    do
      if (coord_space_value(value_index) == emdi) then
        key_from_coord_space = unset_key
        return
      else if (coord_space_value(value_index) == value) then
        key_from_coord_space = coord_space_key(value_index)
        return
      else
        value_index = value_index + 1
        if (value_index > ubound(coord_space_key, 1)) then
          write( log_scratch_space, &
                 '("Value ", I0, " is not in finite_element coord_space")' ) value
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function key_from_coord_space

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  function coord_system_from_key( key ) result (enum_value)

    implicit none

    character(*), intent(in) :: key

    integer(i_def) :: enum_value
    integer(i_def) :: key_index

    enum_value = emdi

    if (key == unset_key) then
      if (log_level() >= LOG_LEVEL_DEBUG ) then
        write(output_unit, '(A)') &
            'Missing key for coord_system enumeration ' // &
            'in finite_element namelist.'
        flush(output_unit)
      end if
      return
    end if

    key_index = 1
    do
      if (trim(coord_system_key(key_index)) == trim(key)) then
        enum_value = coord_system_value(key_index)
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(coord_system_key, 1)) then
          write(error_unit, '(A)') &
              'Key ' // trim(adjustl(key)) // ' not recognised for ' // &
              'coord_system in finite_element namelist.'
          flush(error_unit)
          stop 1
        end if
      end if
    end do

  end function coord_system_from_key

  !> Gets the enumeration key corresponding to a particular value.
  !>
  !> An error is reported if the value is not within range.
  !>
  !> @param[in] value Enumeration value.
  !>
  character(str_def) function key_from_coord_system( value )

    implicit none

    integer(i_def), intent(in) :: value

    integer(i_def) :: value_index

    value_index = 1
    do
      if (coord_system_value(value_index) == emdi) then
        key_from_coord_system = unset_key
        return
      else if (coord_system_value(value_index) == value) then
        key_from_coord_system = coord_system_key(value_index)
        return
      else
        value_index = value_index + 1
        if (value_index > ubound(coord_system_key, 1)) then
          write( log_scratch_space, &
                 '("Value ", I0, " is not in finite_element coord_system")' ) value
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function key_from_coord_system

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] local_rank Rank of current process.
  !> @param [in] scan .true. if reading namelist to acquire scalar
  !>                  values which may possbly be required for
  !>                  array sizing during postprocessing.
  !>
  subroutine read_finite_element_namelist( file_unit, local_rank, scan )

    use constants_mod, only: i_def

    implicit none

    integer(i_def), intent(in) :: file_unit
    integer(i_def), intent(in) :: local_rank
    logical,        intent(in) :: scan

    call read_namelist( file_unit, local_rank, scan, &
                        cellshape, &
                        coord_space, &
                        coord_system )

  end subroutine read_finite_element_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, local_rank, scan, &
                            dummy_cellshape, &
                            dummy_coord_space, &
                            dummy_coord_system )

    implicit none

    integer(i_def), intent(in) :: file_unit
    integer(i_def), intent(in) :: local_rank
    logical,        intent(in) :: scan
    integer(i_def), intent(out) :: dummy_cellshape
    integer(i_def), intent(out) :: dummy_coord_space
    integer(i_def), intent(out) :: dummy_coord_system

    integer(i_def) :: buffer_integer_i_def(7)
    integer(i_def) :: buffer_logical_l_def(2)

    character(str_def) :: cellshape
    character(str_def) :: coord_space
    character(str_def) :: coord_system

    namelist /finite_element/ cellshape, &
                              coord_order, &
                              coord_order_nonprime, &
                              coord_space, &
                              coord_system, &
                              element_order_h, &
                              element_order_v, &
                              rehabilitate, &
                              vorticity_in_w1

    integer(i_def) :: condition

    cellshape = unset_key
    coord_order = imdi
    coord_order_nonprime = imdi
    coord_space = unset_key
    coord_system = unset_key
    element_order_h = imdi
    element_order_v = imdi
    nqp_h_exact = imdi
    nqp_v_exact = imdi
    rehabilitate = .false.
    vorticity_in_w1 = .false.

    if (local_rank == 0) then

      read(file_unit, nml=finite_element, iostat=condition, &
           iomsg=log_scratch_space)

      if (condition /= 0) then
        write(error_unit, '(A)') trim(log_scratch_space)
        flush(error_unit)
        stop 1
      end if

      dummy_cellshape = cellshape_from_key( cellshape )
      dummy_coord_space = coord_space_from_key( coord_space )
      dummy_coord_system = coord_system_from_key( coord_system )

    end if

    buffer_integer_i_def(1) = dummy_cellshape
    buffer_integer_i_def(2) = coord_order
    buffer_integer_i_def(3) = coord_order_nonprime
    buffer_integer_i_def(4) = dummy_coord_space
    buffer_integer_i_def(5) = dummy_coord_system
    buffer_integer_i_def(6) = element_order_h
    buffer_integer_i_def(7) = element_order_v
    buffer_logical_l_def(1) = merge( 1, 0, rehabilitate )
    buffer_logical_l_def(2) = merge( 1, 0, vorticity_in_w1 )

    call global_mpi%broadcast( buffer_integer_i_def, 7, 0 )
    call global_mpi%broadcast( buffer_logical_l_def, 2, 0 )

    dummy_cellshape = buffer_integer_i_def(1)
    coord_order = buffer_integer_i_def(2)
    coord_order_nonprime = buffer_integer_i_def(3)
    dummy_coord_space = buffer_integer_i_def(4)
    dummy_coord_system = buffer_integer_i_def(5)
    element_order_h = buffer_integer_i_def(6)
    element_order_v = buffer_integer_i_def(7)
    rehabilitate = buffer_logical_l_def(1) /= 0
    vorticity_in_w1 = buffer_logical_l_def(2) /= 0

    if (scan) then
      nml_loaded = .false.
    else
      nml_loaded = .true.
    end if

  end subroutine read_namelist


  !> @brief Returns a <<namelist_type>> object populated with the
  !>        current contents of this configuration module.
  !> @return namelist_obj <<namelist_type>> with current namelist contents.
  function get_finite_element_nml() result(namelist_obj)

    implicit none

    type(namelist_type)      :: namelist_obj
    type(namelist_item_type) :: members(11)

    call members(1)%initialise( &
                'cellshape', cellshape )

    call members(2)%initialise( &
                'coord_order', coord_order )

    call members(3)%initialise( &
                'coord_order_nonprime', coord_order_nonprime )

    call members(4)%initialise( &
                'coord_space', coord_space )

    call members(5)%initialise( &
                'coord_system', coord_system )

    call members(6)%initialise( &
                'element_order_h', element_order_h )

    call members(7)%initialise( &
                'element_order_v', element_order_v )

    call members(8)%initialise( &
                'nqp_h_exact', nqp_h_exact )

    call members(9)%initialise( &
                'nqp_v_exact', nqp_v_exact )

    call members(10)%initialise( &
                'rehabilitate', rehabilitate )

    call members(11)%initialise( &
                'vorticity_in_w1', vorticity_in_w1 )

    if (trim(profile_name) /= trim(cmdi) ) then
      call namelist_obj%initialise( trim(listname), &
                                    members, &
                                    profile_name = profile_name )
    else
      call namelist_obj%initialise( trim(listname), &
                                    members )
    end if

  end function get_finite_element_nml

  !> @brief Returns a <<finite_element_nml_type>> object populated with the
  !>        current contents of this configuration module.
  !> @return namelist_obj <<finite_element_nml_type>> with current namelist contents.
  function get_new_finite_element_nml() result(namelist_obj)

    use finite_element_nml_mod, only: finite_element_nml_type

    implicit none

    type(finite_element_nml_type) :: namelist_obj
    type(namelist_item_type) :: members(11)

    call members(1)%initialise( &
                'cellshape', cellshape )

    call members(2)%initialise( &
                'coord_order', coord_order )

    call members(3)%initialise( &
                'coord_order_nonprime', coord_order_nonprime )

    call members(4)%initialise( &
                'coord_space', coord_space )

    call members(5)%initialise( &
                'coord_system', coord_system )

    call members(6)%initialise( &
                'element_order_h', element_order_h )

    call members(7)%initialise( &
                'element_order_v', element_order_v )

    call members(8)%initialise( &
                'nqp_h_exact', nqp_h_exact )

    call members(9)%initialise( &
                'nqp_v_exact', nqp_v_exact )

    call members(10)%initialise( &
                'rehabilitate', rehabilitate )

    call members(11)%initialise( &
                'vorticity_in_w1', vorticity_in_w1 )

    if (trim(profile_name) /= trim(cmdi) ) then
      call namelist_obj%initialise( trim(listname), &
                                    members, &
                                    profile_name = profile_name )
    else
      call namelist_obj%initialise( trim(listname), &
                                    members )
    end if

  end function get_new_finite_element_nml


  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_finite_element_namelist()

    use constants_mod, only: i_def, r_def

    implicit none

    integer(i_def) :: missing_data

    ! Computed fields are resolved after everything has been loaded since they
    ! can refer to fields in other namelists.
    !
    ! Parameter name nqp_h_exact: dereferenced_list_vars are: ['element_order_h']
    missing_data = 0
    if (kind(element_order_h) == r_def) then
       if (real(element_order_h, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(element_order_h) == i_def) then
       if (int(element_order_h, i_def)  == imdi) missing_data = missing_data + 1
    end if

    if ( missing_data >=1 ) then
       nqp_h_exact = imdi
    else
       nqp_h_exact = element_order_h+3
    end if

    ! Parameter name nqp_v_exact: dereferenced_list_vars are: ['element_order_v']
    missing_data = 0
    if (kind(element_order_v) == r_def) then
       if (real(element_order_v, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(element_order_v) == i_def) then
       if (int(element_order_v, i_def)  == imdi) missing_data = missing_data + 1
    end if

    if ( missing_data >=1 ) then
       nqp_v_exact = imdi
    else
       nqp_v_exact = element_order_v+3
    end if

  end subroutine postprocess_finite_element_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function finite_element_is_loadable()

    implicit none

    logical :: finite_element_is_loadable

    if ( multiples_allowed .or. .not. nml_loaded ) then
      finite_element_is_loadable = .true.
    else
      finite_element_is_loadable = .false.
    end if

  end function finite_element_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function finite_element_is_loaded()

    implicit none

    logical :: finite_element_is_loaded

    finite_element_is_loaded = nml_loaded

  end function finite_element_is_loaded

  !> Are multiple finite_element namelists allowed to be read?
  !>
  !> @return True If multiple finite_element namelists are
  !>              permitted.
  !>
  function finite_element_multiples_allowed()

    implicit none

    logical :: finite_element_multiples_allowed

    finite_element_multiples_allowed = multiples_allowed

  end function finite_element_multiples_allowed

  !> Resets the load status to allow
  !> finite_element namelist to be read.
  !>
  subroutine finite_element_reset_load_status()

    implicit none

    nml_loaded = .false.

  end subroutine finite_element_reset_load_status

  !> Clear out any allocated memory
  !>
  subroutine finite_element_final()

    implicit none

    cellshape = emdi
    coord_order = imdi
    coord_order_nonprime = imdi
    coord_space = emdi
    coord_system = emdi
    element_order_h = imdi
    element_order_v = imdi
    nqp_h_exact = imdi
    nqp_v_exact = imdi
    rehabilitate = .false.
    vorticity_in_w1 = .false.

    return
  end subroutine finite_element_final


end module finite_element_config_mod