! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_w2v_wtheta

  ! Description: two functions in an invoke iterating over w2v and
  ! reading from wtheta (both discontinuous)
  use field_mod,        only: field_type
  use testkern_w2v_mod, only: testkern_w2v_type

  implicit none

  type(field_type) :: f1, f2, f3

  call invoke(                    &
       testkern_w2v_type(f1, f2), &
       ! Field f1 has readwrite to read dependence but no halo
       ! exchange is required as w2v is discontinuous
       testkern_w2v_type(f3, f1)  &
          )

end program multikernel_invokes_w2v_wtheta
