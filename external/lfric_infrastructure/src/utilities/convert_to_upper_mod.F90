!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! For further details please refer to the file LICENCE which you should have
! received as part of this distribution.
!-----------------------------------------------------------------------------
module convert_to_upper_mod

   implicit none

   private

   public :: convert_to_upper

contains

!=============================================================================!
!> @brief Changes a string to upper case
!> @param[in] str Input string to convert
!> @result string Upper case string
  pure function convert_to_upper (str) Result (string)

    implicit none
    character(*), intent(in) :: str
    character(len(str))      :: string

    integer :: ic, i

    character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

    string = str
    do i = 1, len_trim(str)
        ic = index(low, str(i:i))
        if (ic > 0) string(i:i) = cap(ic:ic)
    end do

  end function convert_to_upper

end module convert_to_upper_mod
