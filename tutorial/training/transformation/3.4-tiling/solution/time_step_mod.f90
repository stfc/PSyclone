module time_step_mod

    private
    public time_step

contains

    subroutine time_step(current, time_steps)
        use compute_die_mod, only      : compute_die
        use compute_born_mod, only     : compute_born
        use combine_mod, only          : combine
        use count_neighbours_mod, only : count_neighbours
        use output_field_mod, only     : output_field

        implicit none

        ! Declare the fields required
        real(kind=8), dimension(:,:), allocatable, intent(inout) :: current
        real(kind=8), dimension(:,:), allocatable :: die, born, neighbours
        integer, intent(in)            :: time_steps

        integer                        :: time, xstart, xstop, ystart, ystop

        ! Create fields required:
        xstart = lbound(current, 1)
        xstop = ubound(current, 1)
        ystart = lbound(current, 2)
        ystop = ubound(current, 2)

        allocate(neighbours(xstart:xstop, ystart:ystop))
        allocate(die(xstart:xstop, ystart:ystop))
        allocate(born(xstart:xstop, ystart:ystop))

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
