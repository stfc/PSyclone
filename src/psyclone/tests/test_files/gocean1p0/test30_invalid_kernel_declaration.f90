! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

PROGRAM kernel_invalid_declaration_prog

  ! Calls kernel_invalid_declaration which is an invalid kernel as it
  ! does not declare one of the variables that are passed into the
  ! kernel subroutine.
  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_invalid_declaration,  only: compute
  implicit none

  type(r2d_field) :: p_fld, u_fld, cu_fld

  call invoke( compute(cu_fld, p_fld, u_fld) )

END PROGRAM kernel_invalid_declaration_prog
