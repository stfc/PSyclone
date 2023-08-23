module time_step_alg_mod
  implicit none
  private

  public :: time_step

  contains
  subroutine time_step(grid, current, time_steps)
    use output_field_mod, only : output_field
    use grid_mod, only : grid_type
    use field_mod, only : go_t_points, r2d_field
    use psy_time_step_alg_mod, only : invoke_0_count_neighbours, invoke_1_compute_born, invoke_2_compute_die, invoke_3_combine
    TYPE(grid_type), INTENT(IN), TARGET :: grid
    type(r2d_field), intent(inout) :: current
    integer, intent(in) :: time_steps
    type(r2d_field) :: neighbours
    type(r2d_field) :: die
    type(r2d_field) :: born
    integer :: time

    neighbours = r2d_field(grid,go_t_points)
    die = r2d_field(grid,go_t_points)
    born = r2d_field(grid,go_t_points)
    if (time_steps <= 20) then
      call output_field(current)
    end if
    do time = 1, time_steps, 1
      call invoke_0_count_neighbours(neighbours, current)
      call invoke_1_compute_born(born, current, neighbours)
      call invoke_2_compute_die(die, current, neighbours)
      call invoke_3_combine(current, die, born)
      if (time_steps <= 20) then
        call output_field(current)
      end if
    enddo

  end subroutine time_step

end module time_step_alg_mod
