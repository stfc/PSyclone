! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module kern_nested_use_mod
  implicit none

  type, extends(kernel_type) :: kern_nested_use
     type(arg), dimension(1) :: meta_args =              &
          (/ go_arg(GO_READWRITE, GO_CT, GO_POINTWISE) /)
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => kern_nested_use_code
  end type kern_nested_use

contains

  subroutine kern_nested_use_code(i, j, fld)
    use another_mod, only: another_kern
    integer, intent(in) :: i, j
    real(go_wp), dimension(:,:), intent(inout) :: fld

    call another_kern(i, j, fld)

  end subroutine kern_nested_use_code

end module kern_nested_use_mod
