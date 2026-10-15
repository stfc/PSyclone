! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program GameOfLife

    !> @brief The main program, which controls the execution.
    !>
    !> It calls read_config to read the specified configuration file
    !> and initialses dl_esm_inf. Then it executes the time stepping
    !> loop, before shutting down the program.

    use read_config_mod, only: read_config
    use time_step_mod, only  : time_step
    USE grid_mod, only       : grid_type
    USE field_mod, only      : r2d_field

    implicit none

    TYPE(grid_type), target  :: grid
    TYPE(r2d_field)          :: initial

    integer                  :: time_steps

    if (command_argument_count() .ne. 1) then
        print *, "Usage: gol config_file"
        stop
    endif

    ! Read in the initial condition into the field 'current',
    ! and initialise dl_esm_inf.
    call read_config(grid, initial, time_steps) 

    call time_step(grid, initial, time_steps)
end program GameOfLife
