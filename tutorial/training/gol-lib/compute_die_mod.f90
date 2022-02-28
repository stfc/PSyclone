module compute_die_mod

    use kernel_mod
    use argument_mod
    use grid_mod, only: GO_OFFSET_SW
    implicit none

    private
    public compute_die, compute_die_code
    type, extends(kernel_type) :: compute_die
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
         procedure, nopass :: code => compute_die_code
       end type compute_die

contains

    !> @brief Computes which cells die.
    !>
    !> This subroutine computes a field 'die' which has the value '1'
    !> for each cell that is currently alive, but should die. Otherwise
    !> the cell is 0.
    !>
    !> @param[in]  i, j Coordinates of the cell to update.
    !> @param[out] die  The output field with 1 iff the cell dies.
    !> @param[in]  current The current state.
    !> @param[in]  neighbours The number of live neighbours for each cell.
    subroutine compute_die_code(i, j, die, current, neighbours)
        implicit none
        double precision, dimension(:,:), intent(out) :: die
        double precision, dimension(:,:), intent(in)  :: current, neighbours
        integer, intent(in)                           :: i, j

        die(i, j) = 0.0
        ! A cell dies by underpopulation if it has less than 2
        ! neighbours or by overpopulation if it has more than 3
        if (current(i, j) > 0.0 .and.                                   &
            (neighbours(i, j) < 2.0 .or. neighbours(i, j) > 3.0) ) then
              die(i, j) = 1.0
        endif

    end subroutine compute_die_code

end module compute_die_mod