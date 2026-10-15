! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module kern_use_var_mod
  use argument_mod
  use kernel_mod
  use kind_params_mod
  use grid_mod
  implicit none

  type, extends(kernel_type) :: kern_use_var
     type(go_arg), dimension(1) :: meta_args =              &
          (/ go_arg(GO_READWRITE, GO_CT, GO_POINTWISE) /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => kern_use_var_code
  end type kern_use_var

contains

  subroutine kern_use_var_code(i, j, fld)
    use data_mod, only: gravity
    integer, intent(in) :: i, j
    real(go_wp), dimension(:,:), intent(inout) :: fld

    fld(i,j) = gravity * fld(i,j)

  end subroutine kern_use_var_code
  
end module kern_use_var_mod
