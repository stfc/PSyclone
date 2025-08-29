!-------------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-------------------------------------------------------------------------------
!> @brief A collection of hashing algorithms

module hash_mod

  use constants_mod,  only: i_def

implicit none

  private
  public hash_string

contains

  !> @brief Function to create a hash of a string by summing the character values in the string
  !> @param [in] string The string to be summed
  !> @return The sum of the character values in the string
  function hash_string(string) result(ch_sum)
    implicit none
    character(len=*), intent(in)   :: string
    integer(i_def) :: ch_sum
    integer(i_def) :: i
    ch_sum = 0
    do i = 1,len(string)
      ch_sum = ch_sum + ichar(string(i:i))
    end do
  end function hash_string

end module hash_mod