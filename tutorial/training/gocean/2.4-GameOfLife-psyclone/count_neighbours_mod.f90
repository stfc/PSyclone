module count_neighbours_mod

    use kernel_mod, only: GO_INTERNAL_PTS, GO_POINTWISE, kernel_type
    use argument_mod, only: go_arg, GO_CT, GO_READ, GO_STENCIL, GO_WRITE
    use grid_mod, only: GO_OFFSET_SW
    implicit none

    private
    public count_neighbours, count_neighbours_code
    type, extends(kernel_type) :: count_neighbours
       type(go_arg), dimension(TODO) :: meta_args =         &
       TODO
       
       !> This kernel writes to all internal points
       !! of the simulation domain.
       integer :: ITERATES_OVER = GO_INTERNAL_PTS
       integer :: index_offset = GO_OFFSET_SW

       contains
         procedure, nopass :: code => count_neighbours_code
       end type count_neighbours

contains

    !> @brief Computes the new state.
    !>
    !> This subroutine updates the field 'current' by subtracting the dying
    !> cells, and adding the newly born cells.
    !>
    !> @param[in]  i, j       Coordinates of the cell to update.
    !> @param[out] neighbours The number of neighbours for each cell.
    !> @param[out] c          The current state that will be updated.

    subroutine count_neighbours_code(i, j, neighbours, current)
        implicit none
        double precision, dimension(:,:), intent(out) :: neighbours
        double precision, dimension(:,:), intent(in)  :: current
        integer, intent(in)                           :: i, j

        ! Count the neighbours that are alive
        ! TODO: Add the code for counting the eight neighbours
        neighbours(i, j) =


    end subroutine count_neighbours_code

end module count_neighbours_mod