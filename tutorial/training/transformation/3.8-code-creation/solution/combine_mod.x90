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
    !> @param[in]  die     The field with 1 if the cell dies,
    !>                     and 0 otherwise.
    !> @param[in]  born    The field with 1 if a new cell is born,
    !>                     and 0 otherwise.

    subroutine combine(current, die, born)

        implicit none
        real(kind=8), dimension(:,:), allocatable, intent(inout) :: current
        real(kind=8), dimension(:,:), allocatable, intent(in) :: die, born

        integer                        :: xstart, xstop, ystart, ystop
        integer                        :: i, j

        xstart = lbound(current, 1)+1
        xstop = ubound(current, 1)-1
        ystart = lbound(current, 2)+1
        ystop = ubound(current, 2)-1

        do j=ystart, ystop
            do i=xstart, xstop
                current(i, j) = current(i, j) - die(i, j) + born(i, j)
            enddo
        enddo

    end subroutine combine

end module combine_mod