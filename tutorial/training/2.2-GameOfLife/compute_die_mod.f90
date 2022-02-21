module compute_die_mod

    private
    public compute_die

contains

    !> @brief Computes which cells die.
    !>
    !> This subroutine computes a field 'die' which has the value '1'
    !> for each cell that is currently alive, but should die. Otherwise
    !> the cell is 0.
    !>
    !> @param[out] die  The output field with 1 iff the cell dies.
    !> @param[in]  current The current state.
    !> @param[in]  neighbours The number of live neighbours for each cell.

    subroutine compute_die(die, current, neighbours)
        USE grid_mod, only             : grid_type
        USE field_mod, only            : r2d_field

        implicit none
        ! It has to be declared inout - even though the data is only written,
        ! the r2d_field type exists, so it's input as well
        TYPE(r2d_field), intent(inout) :: die
        ! Sorry for the short name, it keeps the line length below shorter
        TYPE(r2d_field), intent(in)    :: current, neighbours

        integer                        :: xstart, xstop, ystart, ystop
        integer                        :: i, j

        xstart = current%internal%xstart
        xstop  = current%internal%xstop
        ystart = current%internal%ystart
        ystop  = current%internal%ystop

        do j=ystart, ystop
            do i=xstart, xstop
                die%data(i, j) = 0.0
                if (current%data(i, j) > 0.0) then
                    ! A cell dies by underpopulation if it has less than 2
                    ! neighbours or by overpopulation if it has more than 3
                    if (neighbours%data(i, j) < 2.0 .or. &
                        neighbours%data(i, j) > 3.0       ) die%data(i, j) = 1.0
                endif
            enddo
        enddo


    end subroutine compute_die

end module compute_die_mod