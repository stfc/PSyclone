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
    !> @param[out] die  The output field with 1 if the cell dies,
    !>                  and 0 otherwise.
    !> @param[in]  current The current state.
    !> @param[in]  neighbours The number of live neighbours for each cell.

    subroutine compute_die(die, current, neighbours)

        implicit none
        ! It has to be declared inout - even though the data is only written,
        ! the r2d_field type exists, so it's input as well
        real(kind=8), dimension(:,:), allocatable, intent(in) :: current, &
                                                                 neighbours
        real(kind=8), dimension(:,:), allocatable, intent(inout) :: die

        integer                        :: xstart, xstop, ystart, ystop
        integer                        :: i, j

        xstart = lbound(current, 1)+1
        xstop = ubound(current, 1)-1
        ystart = lbound(current, 2)+1
        ystop = ubound(current, 2)-1

        do j=ystart, ystop
            do i=xstart, xstop
                die(i, j) = 0.0
                if (current(i, j) > 0.0) then
                    ! A cell dies by underpopulation if it has less than 2
                    ! neighbours or by overpopulation if it has more than 3
                    if (neighbours(i, j) < 2.0 .or. &
                        neighbours(i, j) > 3.0       ) die(i, j) = 1.0
                endif
            enddo
        enddo


    end subroutine compute_die

end module compute_die_mod