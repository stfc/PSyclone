!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------

module flux_direction_mod

  use constants_mod, only : i_native

  implicit none
  private

  integer(i_native), public, parameter :: x_direction = 100
  integer(i_native), public, parameter :: y_direction = 101
  integer(i_native), public, parameter :: z_direction = 102

end module flux_direction_mod
