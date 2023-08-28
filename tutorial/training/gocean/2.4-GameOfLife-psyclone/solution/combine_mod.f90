module combine_mod

    use kernel_mod
    use argument_mod
    use grid_mod, only: GO_OFFSET_SW
    implicit none

    private
    public combine, combine_code
    type, extends(kernel_type) :: combine
       type(go_arg), dimension(3) :: meta_args =           &
            (/ go_arg(GO_READWRITE, GO_CT, GO_POINTWISE),  & ! field
               go_arg(GO_READ,  GO_CT, GO_POINTWISE),      & ! field
               go_arg(GO_READ,  GO_CT, GO_POINTWISE)       & ! field
             /)
       !> This kernel writes to all points of the
       !! simulation domain.
       integer :: ITERATES_OVER = GO_INTERNAL_PTS
       integer :: index_offset = GO_OFFSET_SW

       contains
         procedure, nopass :: code => combine_code
       end type combine

contains

    !> @brief Computes the new state.
    !>
    !> This subroutine updates the field 'current' by subtracting the dying
    !> cells, and adding the newly born cells.
    !>
    !> @param[in]  i, j Coordinates of the cell to update.
    !> @param[out] current The current state that will be updated.
    !> @param[in]  die     The field with 1 iff the cell dies.
    !> @param[in]  born    The field with 1 iff a new cell is born.

    subroutine combine_code(i, j, current, die, born)
        implicit none
        double precision, dimension(:,:), intent(out) :: current
        double precision, dimension(:,:), intent(in)  :: die, born
        integer, intent(in)                           :: i, j

        current(i, j) = current(i, j)  &
                      - die(i, j)      &
                      + born(i, j)

    end subroutine combine_code

end module combine_mod