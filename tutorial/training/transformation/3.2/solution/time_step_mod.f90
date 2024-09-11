module time_step_mod

    private
    public time_step

contains

    subroutine time_step(grid, current, time_steps)
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
        TYPE(r2d_field)                :: neighbours, die, born
        integer                        :: time

        ! Create fields required:
        neighbours = r2d_field(grid, GO_T_POINTS)
        die        = r2d_field(grid, GO_T_POINTS)
        born       = r2d_field(grid, GO_T_POINTS)

        if(time_steps<=20) call output_field(current)
        ! In each time step:
        ! 1. Count neighbours
        ! 2. Compute newly born cells
        ! 3. Compute dying cells.
        ! 4. Combine the results from 2. and 3. with current state
        do time =1, time_steps
            call count_neighbours(neighbours, current)
            call compute_born(born, current, neighbours)
            call compute_die(die, current, neighbours)
            call combine(current, die, born)
            if(time_steps<=20) call output_field(current)
        enddo

    end subroutine time_step

end module time_step_mod
