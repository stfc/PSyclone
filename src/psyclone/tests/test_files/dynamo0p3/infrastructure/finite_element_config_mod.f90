!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the finite_element namelist.
!>
module finite_element_config_mod

  use constants_mod, only: i_def, &
                           i_native, &
                           l_def, &
                           str_def
  use log_mod,       only: log_event, log_scratch_space &
                         , LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, LOG_LEVEL_INFO
  use mpi_mod,       only: broadcast

  use constants_mod, only: cmdi, emdi, imdi, rmdi, unset_key

  implicit none

  private
  public :: cellshape_from_key, key_from_cellshape, &
            coord_system_from_key, key_from_coord_system, &
            read_finite_element_namelist, postprocess_finite_element_namelist, &
            finite_element_is_loadable, finite_element_is_loaded, finite_element_final

  integer(i_native), public, parameter :: cellshape_quadrilateral = 455
  integer(i_native), public, parameter :: cellshape_triangle = 60
  integer(i_native), public, parameter :: coord_system_alphabetaz = 977
  integer(i_native), public, parameter :: coord_system_lonlatz = 622
  integer(i_native), public, parameter :: coord_system_xyz = 271

  integer(i_native), public, protected :: cellshape = emdi
  integer(i_def), public, protected :: coord_order = imdi
  integer(i_native), public, protected :: coord_system = emdi
  integer(i_def), public, protected :: element_order = imdi
  integer(i_def), public, protected :: nqp_exact = imdi
  logical(l_def), public, protected :: rehabilitate = .false.
  logical(l_def), public, protected :: vorticity_in_w1 = .false.

  logical :: namelist_loaded = .false.

  character(str_def), parameter :: cellshape_key(2) &
          = [character(len=str_def) :: 'quadrilateral', &
                                       'triangle']
  character(str_def), parameter :: coord_system_key(3) &
          = [character(len=str_def) :: 'alphabetaz', &
                                       'lonlatz', &
                                       'xyz']

  integer(i_native), parameter :: cellshape_value(2) &
          = [455_i_native, &
             60_i_native]
  integer(i_native), parameter :: coord_system_value(3) &
          = [977_i_native, &
             622_i_native, &
             271_i_native]

contains

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  integer(i_native) function cellshape_from_key( key )

    implicit none

    character(*), intent(in) :: key

    integer(i_native) :: key_index

    if (key == unset_key) then
      write( log_scratch_space, '(A)') &
          'Missing key for cellshape enumeration in finite_element namelist.'
      cellshape_from_key = emdi
      call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      return
    end if

    key_index = 1
    do
      if (trim(cellshape_key(key_index)) == trim(key)) then
        cellshape_from_key = cellshape_value(key_index)
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(cellshape_key, 1)) then
          write( log_scratch_space, &
              '("Key ''", A, "'' not recognised for finite_element cellshape")' ) &
              trim(adjustl(key))
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
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

    integer(i_native), intent(in) :: value

    integer(i_native) :: value_index

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
  integer(i_native) function coord_system_from_key( key )

    implicit none

    character(*), intent(in) :: key

    integer(i_native) :: key_index

    if (key == unset_key) then
      write( log_scratch_space, '(A)') &
          'Missing key for coord_system enumeration in finite_element namelist.'
      coord_system_from_key = emdi
      call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      return
    end if

    key_index = 1
    do
      if (trim(coord_system_key(key_index)) == trim(key)) then
        coord_system_from_key = coord_system_value(key_index)
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(coord_system_key, 1)) then
          write( log_scratch_space, &
              '("Key ''", A, "'' not recognised for finite_element coord_system")' ) &
              trim(adjustl(key))
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
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

    integer(i_native), intent(in) :: value

    integer(i_native) :: value_index

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
  !>
  subroutine read_finite_element_namelist( file_unit, local_rank )

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank

    call read_namelist( file_unit, local_rank, &
                        cellshape, &
                        coord_system )

  end subroutine read_finite_element_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, local_rank, &
                            dummy_cellshape, &
                            dummy_coord_system )

    use constants_mod, only: i_def, r_def

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank
    integer(i_def)                :: missing_data
    integer(i_native), intent(out) :: dummy_cellshape
    integer(i_native), intent(out) :: dummy_coord_system

    integer(i_def) :: buffer_integer_i_def(2)
    integer(i_native) :: buffer_integer_i_native(2)
    integer(i_native) :: buffer_logical_l_def(2)

    character(str_def) :: cellshape
    character(str_def) :: coord_system

    namelist /finite_element/ cellshape, &
                              coord_order, &
                              coord_system, &
                              element_order, &
                              rehabilitate, &
                              vorticity_in_w1

    integer(i_native) :: condition

    missing_data = 0

    cellshape = unset_key
    coord_order = imdi
    coord_system = unset_key
    element_order = imdi
    nqp_exact = imdi
    rehabilitate = .false.
    vorticity_in_w1 = .false.

    if (local_rank == 0) then

      read( file_unit, nml=finite_element, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      dummy_cellshape = cellshape_from_key( cellshape )
      dummy_coord_system = coord_system_from_key( coord_system )

    end if

    buffer_integer_i_native(1) = dummy_cellshape
    buffer_integer_i_def(1) = coord_order
    buffer_integer_i_native(2) = dummy_coord_system
    buffer_integer_i_def(2) = element_order
    buffer_logical_l_def(1) = merge( 1, 0, rehabilitate )
    buffer_logical_l_def(2) = merge( 1, 0, vorticity_in_w1 )

    call broadcast( buffer_integer_i_def, 2, 0 )
    call broadcast( buffer_integer_i_native, 2, 0 )
    call broadcast( buffer_logical_l_def, 2, 0 )

    dummy_cellshape = buffer_integer_i_native(1)
    coord_order = buffer_integer_i_def(1)
    dummy_coord_system = buffer_integer_i_native(2)
    element_order = buffer_integer_i_def(2)
    rehabilitate = buffer_logical_l_def(1) /= 0
    vorticity_in_w1 = buffer_logical_l_def(2) /= 0

   ! Parameter name nqp_exact: dereferenced_list_vars are: ['element_order']
    missing_data = 0
    if (kind(element_order) == r_def) then
       if (real(element_order, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(element_order) == i_def) then
       if (int(element_order, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if ( missing_data >=1 ) then
       nqp_exact = imdi
    else
       nqp_exact = element_order + 3
    end if

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_finite_element_namelist()

    implicit none


  end subroutine postprocess_finite_element_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function finite_element_is_loadable()

    implicit none

    logical :: finite_element_is_loadable

    finite_element_is_loadable = .not. namelist_loaded

  end function finite_element_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function finite_element_is_loaded()

    implicit none

    logical :: finite_element_is_loaded

    finite_element_is_loaded = namelist_loaded

  end function finite_element_is_loaded

  !> Clear out any allocated memory
  !>
  subroutine finite_element_final()

    implicit none

    cellshape = emdi
    coord_order = imdi
    coord_system = emdi
    element_order = imdi
    nqp_exact = imdi
    rehabilitate = .false.
    vorticity_in_w1 = .false.

    return
  end subroutine finite_element_final


end module finite_element_config_mod
