module time_step_alg_mod
  implicit none
  private

  public :: time_step

  contains
  subroutine time_step(grid, current, time_steps)
    use grid_mod, only : grid_type
    use field_mod, only : r2d_field

    TYPE(grid_type), INTENT(IN), TARGET :: grid
    type(r2d_field), intent(inout) :: current
    integer, intent(in) :: time_steps

  end subroutine time_step

end module time_step_alg_mod
