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
    !> @param[out] born The output field with 1 if a cell is newly born,
    !>                  and 0 otherwise.
    !> @param[in]  current The current state.
    !> @param[in]  neighbours The number of live neighbours for each cell.
    subroutine compute_born(born, current, neighbours)

        implicit none
        real(kind=8), dimension(:,:), allocatable, intent(inout) :: born
        real(kind=8), dimension(:,:), allocatable, intent(in) :: current, neighbours

        integer                        :: xstart, xstop, ystart, ystop
        integer                        :: i, j

        xstart = lbound(current, 1)+1
        xstop = ubound(current, 1)-1
        ystart = lbound(current, 2)+1
        ystop = ubound(current, 2)-1

        do j=ystart, ystop
            do i=xstart, xstop
                born(i, j) = 0.0
                if (current(i, j) == 0.0 .and.       &
                    neighbours(i, j) == 3.0   ) then
                    ! A new cell is born in an empty location if
                    ! it has exactly three neighbours.
                    born(i, j) = 1.0
                endif
            enddo
        enddo

    end subroutine compute_born

end module compute_born_mod