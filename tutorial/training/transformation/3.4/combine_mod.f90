module combine_mod

    private
    public combine

contains

    !> @brief Computes the new state.
    !>
    !> This subroutine updates the field 'current' by subtracting the dying
    !> cells, and adding the newly born cells.
    !>
    !> @param[out] current The current state that will be updated
    !> @param[in]  die     The field with 1 iff the cell dies
    !> @param[in]  born    The field with 1 iff a new cell is born

    subroutine combine(current, die, born)
        USE field_mod, only            : r2d_field

        implicit none
        TYPE(r2d_field), intent(inout) :: current
        TYPE(r2d_field), intent(in)    :: die, born

        integer                        :: xstart, xstop, ystart, ystop
        integer                        :: i, j

        xstart = current%internal%xstart
        xstop  = current%internal%xstop
        ystart = current%internal%ystart
        ystop  = current%internal%ystop

        do j=ystart, ystop
            do i=xstart, xstop
                current%data(i, j) = current%data(i, j)  &
                                   - die%data(i, j)      &
                                   + born%data(i, j)
            enddo
        enddo

    end subroutine combine

end module combine_mod