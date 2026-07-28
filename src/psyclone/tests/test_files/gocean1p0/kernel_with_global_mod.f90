! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module kernel_with_global_mod
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  private

  public time_smooth, time_smooth_code

  !> Constant declared in this module but then accessed within kernel - a
  !! problem for in-lining or other kernel transformations.
  real(go_wp) :: alpha
  
  type, extends(kernel_type) :: kernel_with_global
     type(go_arg), dimension(3) :: meta_args = &
          (/ go_arg(GO_READ,      GO_EVERY, GO_POINTWISE),     &
             go_arg(GO_READ,      GO_EVERY, GO_POINTWISE),     &
             go_arg(GO_READWRITE, GO_EVERY, GO_POINTWISE)      &
           /)

     integer :: iterates_over = GO_INTERNAL_PTS  
     integer :: index_offset = GO_OFFSET_ANY

  contains
    procedure, nopass :: code => kernel_with_global_code
  end type kernel_with_global

contains

  !> Kernel which accesses a variable declared in the parent module
  subroutine kernel_with_global_code(i, j, field, field_new, field_old)
    implicit none
    integer,  intent(in)                       :: i, j
    real(go_wp), intent(in),    dimension(:,:) :: field
    real(go_wp), intent(in),    dimension(:,:) :: field_new
    real(go_wp), intent(inout), dimension(:,:) :: field_old

    field_old(i,j) = field(i,j) + &
         alpha*(field_new(i,j) - 2.0d0*field(i,j) + field_old(i,j))

  end subroutine kernel_with_global_code

end module kernel_with_global_mod
