!-----------------------------------------------------------------------------
! (c) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief Accessors for missing data indicators (rmdi, imdi)
!>
module mdi_mod

  use constants_mod, only: r_def, i_def, l_def, rmdi, imdi
  use log_mod,       only: log_event, log_level_warning

  implicit none

  private

  public :: is_rmdi, is_imdi

contains

!> @brief   Check for undefined real number
!> @details Both the current and the previous value of rmdi are
!>          used during transition.
!> @param[in] x real number to be checked
!> @return      result of the check
function is_rmdi(x) result(status)

  implicit none

  real(r_def)    :: x
  logical(l_def) :: status

  if ( x == -huge(0.0_r_def) ) then
    call log_event('obsolete value in is_rmdi', log_level_warning)
  end if

  status = ( x == rmdi .or. x == -huge(0.0_r_def) )

end function is_rmdi

!> @brief   Check for undefined integer
!> @details Both the current and the previous value of imdi are
!>          used during transition.
!> @param[in] n integer to be checked
!> @return      result of the check
function is_imdi(n) result(status)

  implicit none

  integer(i_def) :: n
  logical(l_def) :: status

  if ( n == -huge(0_i_def) ) then
    call log_event('obsolete value in is_imdi', log_level_warning)
  end if

  status = ( n == imdi .or. n == -huge(0_i_def) )

end function is_imdi

end module mdi_mod
