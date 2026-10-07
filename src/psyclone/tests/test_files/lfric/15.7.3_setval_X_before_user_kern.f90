! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (set field values to another field)
  ! called before a user-defined kernel.
  use field_mod, only: field_type
  use testkern_mod, only: testkern_type
  
  implicit none

  type(field_type) :: f1, f2, f3, f4

  call invoke( setval_X(f2, f1),                  &
               testkern_type(aval, f2, f1, f3, f4) )

end program single_invoke
