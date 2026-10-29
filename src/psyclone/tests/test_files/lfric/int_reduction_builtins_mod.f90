! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------
module lfric_builtins_mod

  use argument_mod
  use kernel_mod

  !> Fake built-in that purports to do a reduction into an integer scalar
  type, public, extends(kernel_type) :: X_innerproduct_Y
     private
     type(arg_type) :: meta_args(3) = (/                            &
          arg_type(GH_SCALAR, GH_INTEGER, GH_REDUCTION),            &
          arg_type(GH_FIELD,  GH_REAL,    GH_READ, ANY_SPACE_1),    &
          arg_type(GH_FIELD,  GH_REAL,    GH_READ, ANY_SPACE_1)     &
          /)
     integer :: operates_on = DOF
   contains
     procedure, nopass :: X_innerproduct_Y_code
  end type X_innerproduct_Y

contains

  subroutine X_innerproduct_Y_code()
  end subroutine X_innerproduct_Y_code
  
end module lfric_builtins_mod
