










!-----------------------------------------------------------------------------
! (c) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief A module providing sorting functionality.
!>        (Currently that only consists of a bubble sort)
!>
module sort_mod

  use constants_mod, only: i_def, l_def

  implicit none

  private

  public :: bubble_sort

contains

!>@brief Performs a simple bubble sort on an array
!> @param[in]     length Number of elements to be sorted
!> @param[in,out] array Data that will be sorted
!> @param[in,out] indx Optional index that will also be sorted
  subroutine bubble_sort(length, array, indx)
    implicit none

    integer(i_def),           intent(in)    :: length
    integer(i_def),           intent(inout) :: array(:)
    integer(i_def), optional, intent(inout) :: indx(:)

    logical(l_def) :: swapped   ! flag for if a sort changes the order of data
    integer(i_def) :: i         ! loop index
    integer(i_def) :: swap      ! temporary variable for swapping data

    if (present(indx)) then  ! sort data and index
      do
        swapped = .false.
        do i = 1,length-1
          if(array(i) > array(i+1))then
            swap = array(i)
            array(i) = array(i+1)
            array(i+1) = swap
            swap = indx(i)
            indx(i) = indx(i+1)
            indx(i+1) = swap
            swapped = .true.
          end if
        end do
        if( .not.swapped )exit
      end do
    else  ! just sort data
      do
        swapped = .false.
        do i = 1,length-1
          if(array(i) > array(i+1))then
            swap = array(i)
            array(i) = array(i+1)
            array(i+1) = swap
            swapped = .true.
          end if
        end do
        if( .not.swapped )exit
      end do
    end if

  end subroutine bubble_sort

end module sort_mod

