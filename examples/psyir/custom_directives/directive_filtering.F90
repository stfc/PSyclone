! -----------------------------------------------------------------------------
! Modifications under:
! SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

Program simple_example

    character, dimension(13), parameter :: j = "some_test_par"
    integer :: i
    integer, dimension(1000) :: vals

    do i = 1, 1000
        vals(i) = i
    end do

    ! Don't parallelise this next loop due to the string comparison
    !$my_dir no_par
    do i = 2, 1000
        if(j(3) == "m") then
            vals(i) = vals(i-1) + 3
        else
            vals(i) = vals(i) * 2
        end if
    end do

    do i = 1, 1000
        vals(i) = vals(i) / 2
    end do

End Program simple_example
