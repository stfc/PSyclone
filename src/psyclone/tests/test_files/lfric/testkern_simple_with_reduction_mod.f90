! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module simple_with_reduction_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: simple_with_reduction_type
    type(arg_type), dimension(3) :: meta_args =           &
         (/ arg_type(gh_scalar, gh_real,    gh_reduction),&
            arg_type(gh_field,  gh_real,    gh_read, w1), &
            arg_type(gh_scalar, gh_integer, gh_read) /)
    integer :: operates_on = cell_column
  contains
    procedure, nopass :: code => simple_with_reduction_code
  end type simple_with_reduction_type

contains

  subroutine simple_with_reduction_code()
  end subroutine simple_with_reduction_code

end module simple_with_reduction_mod
