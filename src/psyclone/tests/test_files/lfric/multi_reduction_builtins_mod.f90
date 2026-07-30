! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> @brief Meta-data for the LFRic built-in operations.
!> @details This meta-data is broken for testing purposes.
module lfric_builtins_mod

  use argument_mod
  use kernel_mod

  !> Fake built-in that purports to do two reductions
  type, public, extends(kernel_type) :: X_innerproduct_Y
     private
     type(arg_type) :: meta_args(3) = (/                                 &
          arg_type(GH_FIELD,  GH_REAL, GH_WRITE,    ANY_SPACE_1),        &
          arg_type(GH_SCALAR, GH_REAL, GH_REDUCTION            ),        &
          arg_type(GH_SCALAR, GH_REAL, GH_REDUCTION            )         &
          /)
     integer :: operates_on = DOF
   contains
     procedure, nopass :: setval_c_code
  end type X_innerproduct_Y

contains

  subroutine setval_c_code()
  end subroutine setval_c_code
  
end module lfric_builtins_mod
