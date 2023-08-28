module count_neighbours_mod

    use kernel_mod
    use argument_mod
    use grid_mod, only: GO_OFFSET_SW
    implicit none

    private
    public count_neighbours, count_neighbours_code
    type, extends(kernel_type) :: count_neighbours
       type(go_arg), dimension(2) :: meta_args =         &
            (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),    & ! field
               go_arg(GO_READ,  GO_CT, GO_STENCIL(111,   &
                                                  101,   &
                                                  111))  & ! field
             /)
       !> This kernel writes to all points of the
       !! simulation domain.
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
    !> @param[in]  i, j Coordinates of the cell to update.
    !> @param[out] c    The current state that will be updated.
    !> @param[in]  die  The field with 1 iff the cell dies.
    !> @param[in]  born The field with 1 iff a new cell is born.

    subroutine count_neighbours_code(i, j, neighbours, c)
        implicit none
        double precision, dimension(:,:), intent(out) :: neighbours
        ! Sorry for the short name, it keeps the line length below shorter
        double precision, dimension(:,:), intent(in)  :: c
        integer, intent(in)                           :: i, j

        ! Count the neighbours that are alive
        neighbours(i, j) = c(i-1, j-1) + c(i, j-1) + c(i+1, j-1) &
                         + c(i-1, j  )             + c(i+1, j  ) &
                         + c(i-1, j+1) + c(i, j+1) + c(i+1, j+1)


    end subroutine count_neighbours_code

end module count_neighbours_mod