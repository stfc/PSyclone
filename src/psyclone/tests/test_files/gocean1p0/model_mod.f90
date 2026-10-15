! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! An example module containing some variables and a parameter so that we
! can have test kernels that import them.
module model_mod
  
    use kind_params_mod

    real(go_wp), parameter :: rdt = 1.0
    real(go_wp) :: magic
    real(go_wp) :: cbfr

end module model_mod
