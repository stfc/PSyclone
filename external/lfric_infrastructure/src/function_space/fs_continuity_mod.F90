!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!> @brief Define enumerator variables that describe the different types of
!>        continuity.
!>
!> @details Enumerator variables that describe the different types of continuity
!>          that can be used to construct function spaces

module fs_continuity_mod

  use constants_mod, only : i_def, l_native, str_short
  use log_mod, only : log_event, log_scratch_space, log_level_error

  implicit none

  private
  public :: name_from_functionspace, functionspace_from_name, &
            is_fs_horizontally_continuous, is_fs_vertically_continuous

  character(*), private, parameter :: module_name = 'fs_continuity_mod'

  !-------------------------------------------------------------------------------
  ! Module parameters
  !-------------------------------------------------------------------------------
  integer(i_def), public, parameter :: W0        = 173
  integer(i_def), public, parameter :: W1        = 194
  integer(i_def), public, parameter :: W2        = 889
  integer(i_def), public, parameter :: W2V       = 857
  integer(i_def), public, parameter :: W2H       = 884
  integer(i_def), public, parameter :: W2broken  = 211
  integer(i_def), public, parameter :: W2Hbroken = 112
  integer(i_def), public, parameter :: W2trace   = 213
  integer(i_def), public, parameter :: W2Vtrace  = 666
  integer(i_def), public, parameter :: W2Htrace  = 777
  integer(i_def), public, parameter :: W3        = 424
  integer(i_def), public, parameter :: Wtheta    = 274
  integer(i_def), public, parameter :: Wchi      = 869

  integer(i_def), private, parameter :: num_fs   = 13

  integer(i_def), private, parameter :: fs_enumerator(num_fs, 3) = &
         transpose(reshape( [                                      &
!             enumerator    horizontally    vertically
!                           continuous      continuous
              W0,           1,              1, &
              W1,           1,              1, &
              W2,           1,              1, &
              W2V,          0,              1, &
              W2H,          1,              0, &
              W2broken,     0,              0, &
              W2Hbroken,    0,              0, &
              W2trace,      1,              1, &
              W2Vtrace,     0,              1, &
              W2Htrace,     1,              0, &
              W3,           0,              0, &
              Wtheta,       0,              1, &
              Wchi,         0,              0  &
                            ], [3, num_fs] ) )

  character(str_short), private, parameter :: fs_name(num_fs) = &
         [character(str_short) ::                               &
             'W0',        &
             'W1',        &
             'W2',        &
             'W2V',       &
             'W2H',       &
             'W2broken',  &
             'W2Hbroken', &
             'W2trace',   &
             'W2Htrace',  &
             'W2Vtrace',  &
             'W3',        &
             'Wtheta',    &
             'Wchi' ]

contains

  !> Gets the name corresponding to a particular function space identifier.
  !>
  !> @param[in] fs One of the function space enumerations.
  !>
  !> @return String holding the function space name.
  !>
  function name_from_functionspace(fs)

    implicit none

    integer(i_def), intent(in) :: fs

    character(str_short) :: name_from_functionspace

    integer(i_def) :: fs_index

    fs_index = 1
    do
      if (fs_enumerator(fs_index, 1) == fs) then
        name_from_functionspace = fs_name(fs_index)
        return
      end if
      fs_index = fs_index + 1
      if (fs_index > num_fs) then
        write(log_scratch_space, &
        '(A, ": Unrecognised function space: ",I0)') module_name, fs
        call log_event(log_scratch_space, log_level_error)
      end if
    end do

  end function name_from_functionspace

  !> Gets the function space identifier corresponding to a particular name.
  !>
  !> @param[in] name String holding the function space name.
  !>
  !> @return One of the function space enumerations.
  !>
  function functionspace_from_name(name)

    implicit none

    character(*), intent(in) :: name
    integer(i_def)           :: functionspace_from_name
    integer(i_def)           :: fs_index

    fs_index = 1
    do
      if (fs_name(fs_index) == name) then
        functionspace_from_name = fs_enumerator(fs_index, 1)
        return
      end if

      fs_index = fs_index + 1
      if (fs_index > num_fs) then
        call log_event("Unknown function space " // name, log_level_error)
      end if
    end do

  end function functionspace_from_name

  !> Returns whether the given function space is horizontally continuous.
  !>
  !> @param[in] fs One of the function space enumerations.
  !>
  !> @return True=horizontally continuous, False=horizontally discontinuous
  !>
  function is_fs_horizontally_continuous(fs) result (continuous)

    implicit none

    integer(i_def), intent(in) :: fs
    logical(l_native) :: continuous

    integer(i_def) :: fs_index

    continuous = .false.
    fs_index = 1
    do
      if ( fs_enumerator(fs_index, 1) == fs ) then
        if ( fs_enumerator(fs_index, 2) == 1 ) continuous = .true.
        exit
      end if
      fs_index = fs_index + 1
      if (fs_index > num_fs) then
        write(log_scratch_space, &
        '(A, ": Unrecognised function space: ",I0)') module_name, fs
        call log_event(log_scratch_space, log_level_error)
      end if
    end do

  end function is_fs_horizontally_continuous

  !> Returns whether the given function space is vertically continuous.
  !>
  !> @param[in] fs One of the function space enumerations.
  !>
  !> @return True=vertically continuous, False=vertically discontinuous
  !>
  function is_fs_vertically_continuous(fs) result (continuous)

    implicit none

    integer(i_def), intent(in) :: fs
    logical(l_native) :: continuous

    integer(i_def) :: fs_index

    continuous = .false.
    fs_index = 1
    do
      if ( fs_enumerator(fs_index, 1) == fs ) then
        if ( fs_enumerator(fs_index, 3) == 1 ) continuous = .true.
        exit
      end if
      fs_index = fs_index + 1
      if (fs_index > num_fs) then
        write(log_scratch_space, &
        '(A, ": Unrecognised function space: ",I0)') module_name, fs
        call log_event(log_scratch_space, log_level_error)
      end if
    end do

  end function is_fs_vertically_continuous

end module fs_continuity_mod
