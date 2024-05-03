!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-----------------------------------------------------------------------------
!
!> @brief Define enumerator variables that describe the different types of continuity.
!>
!> @details Enumerator variables that describe the different types of continuity
!>          that can be used to construct function spaces

module fs_continuity_mod

use constants_mod, only: i_native, str_short
use log_mod,       only: log_event, log_scratch_space, log_level_error

implicit none

private
public :: name_from_functionspace

character(*), private, parameter :: module_name = 'fs_continuity_mod'

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------
integer(i_native), public, parameter :: W0        = 173
integer(i_native), public, parameter :: W1        = 194
integer(i_native), public, parameter :: W2        = 889
integer(i_native), public, parameter :: W2V       = 857
integer(i_native), public, parameter :: W2H       = 884
integer(i_native), public, parameter :: W2broken  = 211
integer(i_native), public, parameter :: W2trace   = 213
integer(i_native), public, parameter :: W2Vtrace  = 666
integer(i_native), public, parameter :: W2Htrace  = 777
integer(i_native), public, parameter :: W3        = 424
integer(i_native), public, parameter :: Wtheta    = 274
integer(i_native), public, parameter :: Wchi      = 869

integer(i_native), public, parameter :: fs_enumerator(12) = [W0,       &
                                                             W1,       &
                                                             W2,       &
                                                             W2V,      &
                                                             W2H,      &
                                                             W2broken, &
                                                             W2trace,  &
                                                             W2Vtrace, &
                                                             W2Htrace, &
                                                             W3,       &
                                                             Wtheta,   &
                                                             Wchi]

character(str_short), public, parameter :: fs_name(12) &
         = [character(str_short)    :: 'W0',       &
                                       'W1',       &
                                       'W2',       &
                                       'W2V',      &
                                       'W2H',      &
                                       'W2broken', &
                                       'W2trace',  &
                                       'W2Htrace', &
                                       'W2Vtrace', &
                                       'W3',       &
                                       'Wtheta',   &
                                       'Wchi']

contains

  !> Gets the name corresponding to a particular function space identifier.
  !>
  !> @param[in] fs One of the function space enumerations.
  !>
  !> @return String holding the function space name.
  !>
  function name_from_functionspace( fs )

    implicit none

    integer(i_native), intent(in) :: fs

    character(str_short) :: name_from_functionspace

    integer(i_native) :: fs_index

    fs_index = 1
    do
      if (fs_enumerator(fs_index) == fs) then
        name_from_functionspace = fs_name(fs_index)
        return
      end if
      fs_index = fs_index + 1
      if (fs_index > ubound(fs_enumerator,1)) then
        write( log_scratch_space, &
               '(A, ": Unrecognised function space: ",I0)' ) module_name, fs
        call log_event( log_scratch_space, log_level_error )
      end if
    end do

  end function name_from_functionspace

end module fs_continuity_mod
