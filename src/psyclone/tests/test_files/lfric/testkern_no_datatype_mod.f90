! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Test code where name of derived type is wrong.
module testkern_no_datatype_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: teskern_type
     type(arg_type), dimension(4) :: meta_args =          &
          (/ arg_type(gh_field, gh_real, gh_inc,  w1),    &
             arg_type(gh_field, gh_real, gh_read, w2),    &
             arg_type(gh_field, gh_real, gh_read, w2),    &
             arg_type(gh_field, gh_real, gh_read, w3)     &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_code
  end type teskern_type

contains

  subroutine testkern_code()
  end subroutine testkern_code

end module testkern_no_datatype_mod
