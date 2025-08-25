!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief A module providing r_solver scalar related classes.
!>
!> @details This is a version of a scalar object that can hold an r_solver data
!> value.

module r_solver_scalar_mod

  ! Eventually the precision of scalar data for use in the solver will be set
  ! in a module held within the model (as it is model information). For now,
  ! PSyclone is expecting to "use" the definitions from here, so this is where
  ! it is set
#if (R_SOLVER_PRECISION == 32)
  use scalar_real32_mod, only: r_solver_scalar_type => scalar_real32_type
#else
  use scalar_real64_mod, only: r_solver_scalar_type => scalar_real64_type
#endif

  implicit none
  private

  public :: r_solver_scalar_type

end module r_solver_scalar_mod
