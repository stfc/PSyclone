module compute_die_mod

    use kernel_mod, only: GO_INTERNAL_PTS, GO_POINTWISE, kernel_type
    use argument_mod, only: go_arg, GO_CT, GO_READ, GO_WRITE
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
       !> This kernel writes to all internal points
       !! of the simulation domain.
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
    !> @param[out] die  The output field with 1 if the cell dies,
    !>             and 0 otherwise.
    !> @param[in]  current The current state.
    !> @param[in]  neighbours The number of live neighbours for each cell.
    subroutine compute_die_code(i, j, die, current, neighbours)
        implicit none
        double precision, dimension(:,:), intent(out) :: die
        double precision, dimension(:,:), intent(in)  :: current, neighbours
        integer, intent(in)                           :: i, j

        ! TODO: Set die to 0, then check if a cell is alive and dies
        endif

    end subroutine compute_die_code

end module compute_die_mod