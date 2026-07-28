! -----------------------------------------------------------------------------
! Original under:
! Copyright (c) 2017-2026,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! Modifications under:
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.

! -----------------------------------------------------------------------------
!> Manages the partitioning namelist.
!>
module partitioning_config_mod

  use constants_mod, only: i_def, &
                           str_def
  use log_mod,       only: log_event, log_scratch_space &
                         , LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, LOG_LEVEL_INFO

  implicit none

  private
  public :: panel_decomposition_from_key, key_from_panel_decomposition, &
            read_partitioning_namelist, postprocess_partitioning_namelist, &
            partitioning_is_loadable, partitioning_is_loaded, partitioning_final

  integer(i_def), public, parameter :: panel_decomposition_auto = 621
  integer(i_def), public, parameter :: panel_decomposition_column = 361
  integer(i_def), public, parameter :: panel_decomposition_custom = 529
  integer(i_def), public, parameter :: panel_decomposition_row = 678

  integer(i_def), public, protected :: panel_decomposition
  integer(i_def), public, protected :: panel_xproc
  integer(i_def), public, protected :: panel_yproc

  logical :: namelist_loaded = .false.

  character(str_def), parameter :: panel_decomposition_key(4) &
          = [character(len=str_def) :: 'auto', &
                                       'column', &
                                       'custom', &
                                       'row']

  integer(i_def), parameter :: panel_decomposition_value(4) &
          = [621_i_def, &
             361_i_def, &
             529_i_def, &
             678_i_def]

contains

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  integer(i_def) function panel_decomposition_from_key( key )

    use constants_mod, only: unset_key, imdi

    implicit none

    character(*), intent(in) :: key

    integer(i_def) :: key_index

    if (key == unset_key) then
      write( log_scratch_space, '(A)') &
          'Missing key for panel_decomposition enumeration in partitioning namelist.'
      panel_decomposition_from_key = int(imdi,i_def)
      call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      return
    end if

    key_index = 1
    do
      if (trim(panel_decomposition_key(key_index)) == trim(key)) then
        panel_decomposition_from_key = panel_decomposition_value(key_index)
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(panel_decomposition_key, 1)) then
          write( log_scratch_space, &
                 '("Key ''", A, "'' not recognised for partitioning panel_decomposition")' ) key
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function panel_decomposition_from_key

  !> Gets the enumeration key corresponding to a particular value.
  !>
  !> An error is reported if the value is not within range.
  !>
  !> @param[in] value Enumeration value.
  !>
  character(str_def) function key_from_panel_decomposition( value )

    use constants_mod, only: unset_key, imdi

    implicit none

    integer(i_def), intent(in) :: value

    integer(i_def) :: value_index

    value_index = 1
    do
      if (panel_decomposition_value(value_index) == int(imdi,i_def)) then
        key_from_panel_decomposition = unset_key
        return
      else if (panel_decomposition_value(value_index) == value) then
        key_from_panel_decomposition = panel_decomposition_key(value_index)
        return
      else
        value_index = value_index + 1
        if (value_index > ubound(panel_decomposition_key, 1)) then
          write( log_scratch_space, &
                 '("Value ", I0, " is not in partitioning panel_decomposition")' ) value
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function key_from_panel_decomposition

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] local_rank Rank of current process.
  !>
  subroutine read_partitioning_namelist( file_unit, local_rank )

    implicit none

    integer(i_def), intent(in) :: file_unit
    integer(i_def), intent(in) :: local_rank

    call read_namelist( file_unit, local_rank, &
                        panel_decomposition )

  end subroutine read_partitioning_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, local_rank, &
                            dummy_panel_decomposition )

    use constants_mod, only: cmdi, unset_key, imdi, rmdi

    implicit none

    integer(i_def), intent(in) :: file_unit
    integer(i_def), intent(in) :: local_rank
    integer(i_def), intent(out) :: dummy_panel_decomposition

    character(str_def) :: panel_decomposition

    namelist /partitioning/ panel_decomposition, &
                            panel_xproc, &
                            panel_yproc

    integer(i_def) :: condition

    panel_decomposition = unset_key
    panel_xproc = imdi
    panel_yproc = imdi

    if (local_rank == 0) then

      read( file_unit, nml=partitioning, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      dummy_panel_decomposition = panel_decomposition_from_key( panel_decomposition )

    end if

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_partitioning_namelist()

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none


  end subroutine postprocess_partitioning_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function partitioning_is_loadable()

    implicit none

    logical :: partitioning_is_loadable

    partitioning_is_loadable = .not. namelist_loaded

  end function partitioning_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function partitioning_is_loaded()

    implicit none

    logical :: partitioning_is_loaded

    partitioning_is_loaded = namelist_loaded

  end function partitioning_is_loaded

  !> Clear out any allocated memory
  !>
  subroutine partitioning_final()

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none

    panel_decomposition = int(imdi,i_def)
    panel_xproc = imdi
    panel_yproc = imdi

    return
  end subroutine partitioning_final


end module partitioning_config_mod
