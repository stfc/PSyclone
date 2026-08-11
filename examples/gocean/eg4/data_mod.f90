! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module data_mod
  !> Example of a module providing data that is accessed by other
  !! kernels.
  use kind_params_mod, only: go_wp
  implicit none
  real(go_wp), parameter :: gravity = -9.8
  real(go_wp), parameter :: friction = 0.1

contains

  function my_function(val)
    real(go_wp), intent(in) :: val
    real(go_wp) :: my_function

    my_function = 2.0*val
    
  end function my_function
  
end module data_mod
