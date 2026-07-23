module count_neighbours_mod

    private
    public count_neighbours

contains
    !> @brief Counts how many live cell surround each cell.
    !>
    !> This subroutine computes a field 'neighbours', storing for
    !> each cell how many of its 8 neighbours are live.
    !>
    !> @param[out] neighbours On exit contains the number of alive neighbours.
    !> @param[in]  current    The current state.

    subroutine count_neighbours(neighbours, current)
        USE field_mod, only            : r2d_field

        implicit none
        ! It has to be declared inout - even though the data is only written,
        ! the r2d_field type exists, so it's input as well
        TYPE(r2d_field), intent(inout) :: neighbours
        TYPE(r2d_field), intent(in)    :: current

        integer                        :: xstart, xstop, ystart, ystop
        integer                        :: i, j

        ! The outer elements is the 'boundary' area, which is set to 0.
        ! We only loop over the internal points
        xstart = current%internal%xstart
        xstop  = current%internal%xstop
        ystart = current%internal%ystart
        ystop  = current%internal%ystop

        do j=ystart, ystop
            do i=xstart, xstop
                ! Compute neighbours to be the sum of the 8 neighbours
            enddo
        enddo

    end subroutine count_neighbours

end module count_neighbours_mod

