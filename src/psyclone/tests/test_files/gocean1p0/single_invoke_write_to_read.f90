! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

PROGRAM write_to_read

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system.

  use field_mod
  use kernel_sw_offset_cu_mod,  only: compute_u
  use kernel_sw_offset_cv_mod,  only: compute_v
  implicit none

  !> Pressure at current time step
  type(r2d_field) :: p_fld
  !> Mass flux in {x,y} direction at current time step
  type(r2d_field) :: cu_fld, cv_fld

  call invoke( compute_u(cu_fld, cv_fld, p_fld), &
               compute_v(cv_fld, cu_fld, p_fld) )

END PROGRAM write_to_read
