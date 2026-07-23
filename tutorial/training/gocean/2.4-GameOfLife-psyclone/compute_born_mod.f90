module compute_born_mod

    use kernel_mod, only: GO_INTERNAL_PTS, GO_POINTWISE, kernel_type
    use argument_mod, only: go_arg, GO_CT, GO_READ, GO_WRITE
    use grid_mod, only: GO_OFFSET_SW
    implicit none

    private
    public compute_born, compute_born_code
    type, extends(kernel_type) :: compute_born
       type(go_arg), dimension(TODO) :: meta_args =
           ! TODO: Declare the three kernel arguments of the kernel. It takes
           ! three fields as argument, the first one 'born' a written field
           ! that contains if a new cell was born, the second is a read field
           ! that contains the current state (1 live, 0 empty), and then a
           ! read field that contains the number of neighbours.

       ! TODO: Declare this kernel to operate on the GO_INTERNAL_PTS
       integer
       ! TODO: Declare that this kernel uses GO_OFFSET_SW
       integer

       contains
          ! TODO: Declare code as the actual kernel procedure
          procedure, nopass
       end type compute_born

contains

    !> @brief Computes which empty fields will get a new cell.
    !>
    !> This subroutine computes a field 'born' which has the value '1'
    !> for each dead cell, but which will get a new cell. Otherwise
    !> the field is 0.
    !>
    !> @param[in]  i, j Coordinates of the cell to update.
    !> @param[out] born The output field with 1 if a cell is newly born,
    !>             and 0 otherwise.
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