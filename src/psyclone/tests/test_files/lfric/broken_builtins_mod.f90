! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------
module broken_builtins_mod

  use argument_mod
  use kernel_mod

  !> field3 = a*field1 + b*field2
  type, public, extends(kernel_type) :: aX_plus_bY
     private
     type(arg_type) :: meta_args(5) = (/                              &
          arg_type(GH_FIELD,  GH_REAL, GH_WRITE, ANY_SPACE_1),        &
          arg_type(GH_SCALAR, GH_REAL, GH_READ              ),        &
          arg_type(GH_FIELD,  GH_REAL, GH_READ,  ANY_SPACE_1),        &
          arg_type(GH_SCALAR, GH_REAL, GH_READ              ),        &
          arg_type(GH_FIELD,  GH_REAL, GH_READ,  ANY_SPACE_1)         &
          /)
     integer :: operates_on = DOF
   contains
     procedure, nopass :: aX_plus_bY_code
  end type blah ! BROKEN

  !> field3 = a*field1 + field2
  type, public, extends(kernel_type) :: aX_plus_Y
     private
     type(arg_type) :: meta_args(4) = (/                              &
          arg_type(GH_FIELD,  GH_REAL, GH_WRITE, ANY_SPACE_1),        &
          arg_type(GH_SCALAR, GH_REAL, GH_READ              ),        &
          arg_type(GH_FIELD,  GH_REAL, GH_READ,  ANY_SPACE_1),        &
          arg_type(GH_FIELD,  GH_REAL, GH_READ,  ANY_SPACE_1)         &
          /)
     integer :: operates_on = DOF
   contains
     procedure, nopass :: aX_plus_Y_code
  end type aX_plus_Y

contains

  subroutine aX_plus_Y_code()
  end subroutine aX_plus_Y_code
  
end module broken_builtins_mod
