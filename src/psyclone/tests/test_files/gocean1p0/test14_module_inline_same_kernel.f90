! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------
PROGRAM module_inline_same_kernel

use kind_params_mod
  use grid_mod
  use field_mod
  use compute_cu_mod, only: compute_cu
  implicit none

  type(r2d_field) :: a, b, c, d
  
  call invoke( compute_cu(a, b, c), compute_cu(b, c, d) )

END PROGRAM module_inline_same_kernel
