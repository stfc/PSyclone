
!> @brief This is a dummy wrapper to allow compilation only.
!>
!> Each exercise will have its own local copy of the time stepping
!> loop. But in order to compiler the gol library, a correct mod
!> file is required by gol.f90, which this file does provide.
!> This code is not used otherwise, the time_step function will
!> be taken from the actual exercises.

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
