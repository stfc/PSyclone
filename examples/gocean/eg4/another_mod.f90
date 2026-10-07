! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module another_mod
  use kind_params_mod, only: go_wp

contains

  subroutine another_kern(i, j, fld)
    !> A kernel that accesses a variable from another module
    use data_mod, only: gravity
    integer, intent(in) :: i, j
    real(go_wp), dimension(:,:) :: fld
    fld(i,j) = i*gravity
  end subroutine another_kern
  
end module another_mod
