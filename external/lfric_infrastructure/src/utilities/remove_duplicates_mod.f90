










!-----------------------------------------------------------------------------
! (c) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief A simple module to remove duplicate entries from array inputs.
!>
module remove_duplicates_mod

  use constants_mod, only: i_def, imdi, str_def, cmdi

  implicit none

  private

  interface remove_duplicates
    module procedure get_unique_int_array
    module procedure get_unique_char_array
  end interface remove_duplicates

  interface any_duplicates
    module procedure any_int_duplicates
    module procedure any_char_duplicates
  end interface any_duplicates

  !> @brief Function call for integer and character arrays
  !> @param[in] array_in  Integer/Character array to remove duplicates from
  !> @returns   array_out Pointer to processed array with duplicate removed

  public :: remove_duplicates, any_duplicates

contains

!-----------------------------------------------------------------------------
! Private function to remove duplicates from integer arrays
!-----------------------------------------------------------------------------
function get_unique_int_array(array_in) result(array_out)

  implicit none

  integer(i_def), intent(in)  :: array_in(:)
  integer(i_def), allocatable :: array_out(:)
  integer(i_def) :: n_entries, i, j, n_uniques

  integer(i_def), allocatable :: unique_list(:)

  n_entries = size(array_in)
  allocate(unique_list(n_entries))
  unique_list = imdi

  unique_list(1) = array_in(1)
  n_uniques = 1

  do i=1, n_entries
    if (array_in(i) == imdi) exit
    do j=1, n_uniques
      if ( array_in(i) == unique_list(j) ) then
        exit
      else
        if (j == n_uniques) then
          unique_list(j+1) = array_in(i)
          n_uniques = n_uniques+1
        end if
      end if
    end do
  end do

  if (allocated(array_out)) deallocate(array_out)
  allocate(array_out(n_uniques))
  array_out(:) = unique_list(1:n_uniques)

  ! Check that if there is only 1 unique item, it isn't imdi
  if ( size(array_out) == 1 ) then
    if (array_out(1) == imdi) deallocate(array_out)
  end if
  if (allocated(unique_list)) deallocate (unique_list)
  return
end function get_unique_int_array

!-----------------------------------------------------------------------------
! Private function to remove duplicates from character arrays
!-----------------------------------------------------------------------------
function get_unique_char_array(array_in) result(array_out)

  implicit none

  character(str_def), intent(in)  :: array_in(:)
  character(str_def), allocatable :: array_out(:)

  integer(i_def) :: n_entries, i, j, n_uniques

  character(str_def), allocatable :: unique_list(:)

  n_entries = size(array_in)
  allocate(unique_list(n_entries))
  unique_list = ''

  unique_list(1) = array_in(1)
  n_uniques = 1

  do i=1, n_entries
    if (array_in(i) == '') exit
    do j=1, n_uniques
      if ( array_in(i) == unique_list(j) .or. &
           array_in(i) == '' .or.             &
           trim(array_in(i)) == trim(cmdi) ) then
        exit
      else
        if (j == n_uniques) then
          unique_list(j+1) = array_in(i)
          n_uniques = n_uniques+1
        end if
      end if
    end do
  end do

  if (allocated(array_out)) deallocate(array_out)
  allocate(array_out(n_uniques))
  array_out(:) = unique_list(1:n_uniques)

  ! Check that if there is only 1 unique item
  ! it isn't cmdi or empty
  if ( size(array_out) == 1 ) then
    if ( array_out(1) == cmdi .or. &
         array_out(1) == '' ) then
       deallocate(array_out)
    end if
  end if

  if (allocated(unique_list)) deallocate (unique_list)
  return
end function get_unique_char_array

!-----------------------------------------------------------------------------
! Private function to check for duplicates from integer arrays
!-----------------------------------------------------------------------------
function any_int_duplicates(array_in) result(answer)

  implicit none

  integer(i_def), intent(in) :: array_in(:)

  integer(i_def) :: n_entries, i

  logical :: answer

  n_entries = size(array_in)
  answer = .false.

  do i=1, n_entries
    if (count(array_in(i) == array_in) > 1) answer=.true.
  end do

  return
end function any_int_duplicates


!-----------------------------------------------------------------------------
! Private function to remove duplicates from character arrays
!-----------------------------------------------------------------------------
function any_char_duplicates(array_in) result(answer)

  implicit none

  character(str_def), intent(in) :: array_in(:)

  integer(i_def) :: n_entries, i

  logical :: answer

  n_entries = size(array_in)
  answer = .false.

  do i=1, n_entries
    if (trim(array_in(i)) /= trim(cmdi)) then
      if (count(array_in(i) == array_in) > 1) answer=.true.
    end if
  end do

  return
end function any_char_duplicates


end module remove_duplicates_mod

