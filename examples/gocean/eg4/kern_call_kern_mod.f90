! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module kern_call_kern_mod
  use kind_params_mod, only: go_wp
  implicit none

  type, extends(kernel_type) :: kern_call_kern
     type(arg), dimension(1) :: meta_args =              &
          (/ go_arg(GO_READWRITE, GO_CT, GO_POINTWISE) /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     integer :: INDEX_OFFSET = GO_OFFSET_SW

  contains
    procedure, nopass :: code => kern_call_kern_code
  end type kern_call_kern

contains

  subroutine kern_call_kern_code(i, j, fld)
    use data_mod, only: my_function
    integer, intent(in) :: i, j
    real(go_wp), dimension(:,:), intent(inout) :: fld

    fld(i,j) = my_function(fld(i,j))

  end subroutine kern_call_kern_code

end module kern_call_kern_mod
