! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------
module lfric_builtins_mod

  use argument_mod
  use kernel_mod

  !> field1 = ascalar
  type, public, extends(kernel_type) :: setval_c
     private
     type(arg_type) :: meta_args(2) = (/                              &
          arg_type(GH_FIELD,  GH_REAL, GH_INC, ANY_SPACE_1),          &
          arg_type(GH_SCALAR, GH_REAL, GH_READ            )           &
          /)
     ! Deliberately BREAK the meta-data - we only support operates_on
     ! DOFS for built-ins in the LFRic API
     integer :: operates_on = CELL_COLUMN
   contains
     procedure, nopass :: setval_c_code
  end type setval_c

contains

  subroutine setval_c_code()
  end subroutine setval_c_code
  
end module lfric_builtins_mod
