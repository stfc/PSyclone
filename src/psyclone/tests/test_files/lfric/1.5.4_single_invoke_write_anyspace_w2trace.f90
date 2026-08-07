! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_fs

  ! Description: single function that writes to fields on ANY_SPACE
  ! and W2trace (both continuous).
  use field_mod,                      only: field_type
  use testkern_write_any_w2trace_mod, only: testkern_write_any_w2trace_type

  implicit none

  type(field_type) :: f1, f2, f3, f4, m1, m2, m3, m4

  call invoke( testkern_write_any_w2trace_type(f1, m1, m2, f2, f3, f4, m3, m4) )

end program single_invoke_fs
