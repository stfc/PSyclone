! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_fs

  ! Description: single function that writes to both an operator and
  ! a field specified in an invoke call
  use constants_mod, only: i_def
  use field_mod,     only: field_type
  use operator_mod,  only: operator_type
  use testkern_write_op_and_fld_mod, &
                     only: testkern_write_op_and_fld_type

  implicit none

  type(field_type)    :: f1(3)
  type(operator_type) :: op1
  integer(i_def)      :: an_int

  call invoke( testkern_write_op_and_fld_type(f1, an_int, op1) )

end program single_invoke_fs
