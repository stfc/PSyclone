! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_multi_anyw2

  ! Description: test that correct code is produced when we have multiple
  ! any_w2 function spaces in a kernel call
  use field_mod,                only: field_type
  use testkern_multi_anyw2_mod, only: testkern_multi_anyw2_type

  implicit none

  type(field_type) :: f1, f2, f3

  call invoke(                               &
       testkern_multi_anyw2_type(f1, f2, f3) &
          )

end program single_invoke_multi_anyw2
