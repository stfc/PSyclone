! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------
module setval_field_any_kernel_mod

  use argument_mod,      only: arg_type,          &
                               GH_FIELD, GH_REAL, &
                               GH_SCALAR,         &
                               GH_INC, GH_READ,   &
                               ANY_SPACE_1, CELL_COLUMN
  use constants_mod,     only: r_def, i_def
  use kernel_mod,        only: kernel_type

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! The type declaration for the kernel. Contains the metadata needed by
  ! the PSy layer.
  !-----------------------------------------------------------------------------
  type, public, extends(kernel_type) :: setval_field_any_kernel_type
    private
    type(arg_type), dimension(2) :: meta_args = (/          &
         arg_type(GH_FIELD,  GH_REAL, GH_INC, ANY_SPACE_1), &
         arg_type(GH_SCALAR, GH_REAL, GH_READ)              &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: code => setval_field_any_code
  end type setval_field_any_kernel_type

  public setval_field_any_code

  contains

  subroutine setval_field_any_code()

  end subroutine setval_field_any_code

end module setval_field_any_kernel_mod
