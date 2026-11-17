! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module simple_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: simple_type
    type(arg_type), dimension(1) :: meta_args = &
         (/ arg_type(gh_field, gh_real, gh_inc, w1) /)
    integer :: operates_on = cell_column
  contains
    procedure, nopass :: code => simple_code
  end type simple_type

contains

  subroutine simple_code()
  end subroutine

end module simple_mod
