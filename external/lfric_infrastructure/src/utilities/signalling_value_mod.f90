!-----------------------------------------------------------------------------
! (C) Crown copyright 2024 Met Office. All rights reserved.
! For further details please refer to the file LICENCE which you should have
! received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief A module for returning known "bad" values, used to initialise fields,
!>        so it becomes obvious when they are used before they are set.
module signalling_value_mod

use, intrinsic :: iso_fortran_env, only : real32, real64, int32
use, intrinsic :: ieee_arithmetic, only: ieee_value, IEEE_SIGNALING_NAN

implicit none

private

public :: get_signalling_value

interface get_signalling_value
  module procedure get_real32_signalling_value, get_real64_signalling_value, &
                   get_int32_signalling_value
end interface get_signalling_value

contains

!> @brief retuns a 32-bit read signalling NaN
!> @param[in] type_variable 32-bit real variable that determines the type of
!>                          the value returned
!> @result signalling_value 32-bit signalling NaN
function get_real32_signalling_value(type_variable) result(signalling_value)
real(real32), intent(in) :: type_variable
real(real32) :: signalling_value
signalling_value = ieee_value(type_variable, IEEE_SIGNALING_NAN)
end function get_real32_signalling_value

!> @brief retuns a 64-bit read signalling NaN
!> @param[in] type_variable 64-bit real variable that determines the type of
!>                          the value returned
!> @result signalling_value 64-bit signalling NaN
function get_real64_signalling_value(type_variable) result(signalling_value)
real(real64), intent(in) :: type_variable
real(real64) :: signalling_value
signalling_value = ieee_value(type_variable, IEEE_SIGNALING_NAN)
end function get_real64_signalling_value

!> @brief retuns a 32-bit integer "bad" value - here, -huge(integer)
!> @param[in] type_variable 32-bit integer variable that determines the type of
!>                          the value returned
!> @result signalling_value 32-bit integer "bad" value - here, -huge(integer)
function get_int32_signalling_value(type_variable) result(signalling_value)
integer(int32), intent(in) :: type_variable
integer(int32) :: signalling_value
signalling_value = -huge(type_variable)
end function get_int32_signalling_value

end module signalling_value_mod
