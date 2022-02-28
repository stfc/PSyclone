module compute_born_mod

    use kernel_mod
    use argument_mod
    use grid_mod, only: GO_OFFSET_SW
    implicit none

    private
    public compute_born, compute_born_code
    type, extends(kernel_type) :: compute_born
       type(go_arg), dimension(3) :: meta_args =         &
            (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),    & ! field
               go_arg(GO_READ,  GO_CT, GO_POINTWISE),    & ! field
               go_arg(GO_READ,  GO_CT, GO_POINTWISE)     & ! field
             /)
       !> This kernel writes to all points of the
       !! simulation domain.
       integer :: ITERATES_OVER = GO_INTERNAL_PTS
       integer :: index_offset = GO_OFFSET_SW

       contains
         procedure, nopass :: code => compute_born_code
       end type compute_born

contains

    !> @brief Computes which empty fields will get a new cell.
    !>
    !> This subroutine computes a field 'born' which has the value '1'
    !> for each dead cell, but which will get a new cell. Otherwise
    !> the field is 0.
    !>
    !> @param[in]  i, j Coordinates of the cell to update.
    !> @param[out] born The output field with 1 iff a cell is newly born.
    !> @param[in]  current The current state.
    !> @param[in]  neighbours The number of live neighbours for each cell.
    subroutine compute_born_code(i, j, born, current, neighbours)

        implicit none
        double precision, dimension(:,:), intent (out)  :: born
        double precision, dimension(:,:), intent(in)    :: current, neighbours
        integer, intent(in)                             :: i, j

        born(i, j) = 0.0
        ! A new cell is born in an empty location if
        ! it has exactly three neighbours.
        if (current(i, j) == 0.0 .and. neighbours(i,j) == 3.0) then
            born(i, j) = 1.0
        endif

    end subroutine compute_born_code

end module compute_born_mod