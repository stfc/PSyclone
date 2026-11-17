! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program GameOfLife

    use parallel_mod, only     : parallel_finalise
    use read_config_mod, only  : read_config
    use time_step_alg_mod, only: time_step
    USE grid_mod, only         : grid_type
    USE field_mod, only        : r2d_field

    implicit none

    TYPE(grid_type), target    :: grid
    TYPE(r2d_field)            :: initial

    integer                    :: time_steps

    if (command_argument_count() .ne. 1) then
        print *, "Usage: gol config_file"
        stop
    endif

    ! Read in the initial condition into the field 'current',
    ! and initialise dl_esm_inf.
    call read_config(grid, initial, time_steps) 

    call time_step(grid, initial, time_steps)
    call parallel_finalise()
end program GameOfLife

