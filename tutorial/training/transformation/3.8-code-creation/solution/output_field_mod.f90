module output_field_mod

    private
    public output_field

contains

    subroutine output_field(field)

        !> @brief Outputs the status of the game of life.
        !>
        !> This subroutine collects the (potentially local) values of
        !> the field onto the master process, and prints the state.
        !>
        !> @param[in] field The state of the Game of Life: 1 for a
        !>                  cell that is alive, 0 otherwise.

        implicit none
        real(kind=8), dimension(:,:), allocatable :: field

        integer                                   :: j

        ! Note that the field has an outer halo, so we only
        ! print the inner region (ignoring the first and last
        ! row and column).
        do j=lbound(field, 2)+1, ubound(field, 2)-1
            write(*,"(99F2.0)") field(2:ubound(field,1)-1, j)
        enddo
        write(*,*)

    end subroutine output_field

end module output_field_mod