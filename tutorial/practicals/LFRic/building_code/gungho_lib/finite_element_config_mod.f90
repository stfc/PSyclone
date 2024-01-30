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
!> Manages the finite_element namelist.
!>
module finite_element_config_mod

  use constants_mod, only: i_def, &
                           i_native, &
                           l_def, &
                           str_def
  use log_mod,       only: log_event, log_scratch_space &
                         , LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, LOG_LEVEL_INFO

  implicit none

  private
  public :: cellshape_from_key, key_from_cellshape, &
            read_finite_element_namelist, postprocess_finite_element_namelist, &
            finite_element_is_loadable, finite_element_is_loaded, finite_element_final

  integer(i_native), public, parameter :: cellshape_quadrilateral = 976
  integer(i_native), public, parameter :: cellshape_triangle = 983

  integer(i_native), public, protected :: cellshape
  integer(i_def), public, protected :: coordinate_order
  integer(i_def), public, protected :: element_order
  integer(i_def), public, protected :: nqp_exact
  logical(l_def), public, protected :: rehabilitate
  logical(l_def), public, protected :: vorticity_in_w1

  logical :: namelist_loaded = .false.

  character(str_def), parameter :: cellshape_key(2) &
          = [character(len=str_def) :: 'quadrilateral', &
                                       'triangle']

  integer(i_native), parameter :: cellshape_value(2) &
          = [976_i_native, &
             983_i_native]

contains

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  integer(i_native) function cellshape_from_key( key )

    use constants_mod, only: unset_key, imdi

    implicit none

    character(*), intent(in) :: key

    integer(i_native) :: key_index

    if (key == unset_key) then
      write( log_scratch_space, '(A)') &
          'Missing key for cellshape enumeration in finite_element namelist.'
      cellshape_from_key = int(imdi,i_native)
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
                 '("Key ''", A, "'' not recognised for finite_element cellshape")' ) key
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

    use constants_mod, only: unset_key, imdi

    implicit none

    integer(i_native), intent(in) :: value

    integer(i_native) :: value_index

    value_index = 1
    do
      if (cellshape_value(value_index) == int(imdi,i_native)) then
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
                        cellshape )

  end subroutine read_finite_element_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, local_rank, &
                            dummy_cellshape )

    use constants_mod, only: cmdi, unset_key, imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank
    integer(i_native), intent(out) :: dummy_cellshape

    integer(i_def) :: buffer_integer_i_def(2)
    integer(i_native) :: buffer_integer_i_native(1)
    integer(i_native) :: buffer_logical_l_def(2)

    character(str_def) :: cellshape

    namelist /finite_element/ cellshape, &
                              coordinate_order, &
                              element_order, &
                              rehabilitate, &
                              vorticity_in_w1

    integer(i_native) :: condition

    cellshape = unset_key
    coordinate_order = imdi
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

    end if

    buffer_integer_i_native(1) = dummy_cellshape
    buffer_integer_i_def(1) = coordinate_order
    buffer_integer_i_def(2) = element_order
    buffer_logical_l_def(1) = merge( 1, 0, rehabilitate )
    buffer_logical_l_def(2) = merge( 1, 0, vorticity_in_w1 )

    dummy_cellshape = buffer_integer_i_native(1)
    coordinate_order = buffer_integer_i_def(1)
    element_order = buffer_integer_i_def(2)
    rehabilitate = buffer_logical_l_def(1) /= 0
    vorticity_in_w1 = buffer_logical_l_def(2) /= 0

    if ( any([element_order] == imdi) .or. &
         any([element_order] == rmdi) ) then
      nqp_exact = imdi
    else
      nqp_exact = element_order + 3
    end if

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_finite_element_namelist()

    use constants_mod, only: cmdi, emdi, imdi, rmdi

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

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none

    cellshape = int(imdi,i_native)
    coordinate_order = imdi
    element_order = imdi
    nqp_exact = imdi
    rehabilitate = .false.
    vorticity_in_w1 = .false.

    return
  end subroutine finite_element_final


end module finite_element_config_mod
