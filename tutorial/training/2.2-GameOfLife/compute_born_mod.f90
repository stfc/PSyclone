module compute_born_mod

    private
    public compute_born

contains

    !> @brief Computes which empty fields will get a new cell.
    !>
    !> This subroutine computes a field 'born' which has the value '1'
    !> for each dead cell, but which will get a new cell. Otherwise
    !> the field is 0.
    !>
    !> @param[out] born The output field with 1 iff a cell is newly born.
    !> @param[in]  current The current state.
    !> @param[in]  neighbours The number of live neighbours for each cell.
    subroutine compute_born(born, current, neighbours)
        USE grid_mod, only             : grid_type
        USE field_mod, only            : r2d_field

        implicit none
        ! It has to be declared inout - even though the data is only written,
        ! the r2d_field type exists, so it's input as well
        TYPE(r2d_field), intent(inout) :: born
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
                born%data(i, j) = 0.0
                if (current%data(i, j) == 0.0 .and.       &
                    neighbours%data(i, j) == 3.0   ) then
                    ! A new cell is born in an empty location if
                    ! it has exactly three neighbours.
                    born%data(i, j) = 1.0
                endif
            enddo
        enddo

    end subroutine compute_born

end module compute_born_mod