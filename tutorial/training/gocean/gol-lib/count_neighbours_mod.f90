module count_neighbours_mod

    use kernel_mod, only: GO_INTERNAL_PTS, GO_POINTWISE, kernel_type
    use argument_mod, only: go_arg, GO_CT, GO_READ, GO_STENCIL, GO_WRITE
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
    !> This subroutine computes a field 'neighbours', storing for
    !> each cell how many of its 8 neighbours are live in the field
    !> 'c'.
    !>
    !> @param[in]  i, j       Coordinates of the cell to update.
    !> @param[out] neighbours On exit contains the number of alive neighbours.
    !> @param[out] c          The current state that will be updated.

    pure subroutine count_neighbours_code(i, j, neighbours, c)
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