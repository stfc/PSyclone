module time_step_mod

    private
    public time_step

contains

    subroutine time_step(grid, current, time_steps)

        !> @brief The time stepping loop.
        !>
        !> This subroutine contains the time stepping loop.
        !> It calls the functions to count the neighbours, computes
        !> which cell die or become alive, and updates the state
        !> - all in a loop over the number of time steps.
        !> If the number of time steps is 20 or less, the state
        !> will be printed out after each time step (useful for
        !> debugging). If more time steps are done, it is assumed
        !> to be a performance test, and no status is printed out.

        use compute_die_mod, only      : compute_die
        use compute_born_mod, only     : compute_born
        use combine_mod, only          : combine
        use count_neighbours_mod, only : count_neighbours
        use output_field_mod, only     : output_field
        USE grid_mod, only             : grid_type
        USE field_mod, only            : r2d_field, go_t_points

        implicit none
        TYPE(grid_type), intent(in), target &
                                       :: grid
        TYPE(r2d_field), intent(inout) :: current
        integer, intent(in)            :: time_steps

        ! Declare the fields required
        TYPE(r2d_field)                :: neighbours
        integer                        :: time

        ! Create fields required:
        neighbours = r2d_field(grid, GO_T_POINTS)

        ! Output the initial state
        if(time_steps<=20) call output_field(current)
        ! In each time step:
        ! 1. Count neighbours
        ! 2. Compute newly born cells
        ! 3. Compute dying cells.
        ! 4. Combine the results from 2. and 3. with current state
        do time =1, time_steps
            call count_neighbours()
            call compute_born()
            call compute_die()
            call combine()
            if(time_steps<=20) call output_field(current)
        enddo

    end subroutine time_step

end module time_step_mod
