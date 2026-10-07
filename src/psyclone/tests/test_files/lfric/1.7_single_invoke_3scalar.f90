! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Single function specified in an invoke call that passes a 'real',
  ! an 'integer' and a 'logical' scalar argument to a kernel
  use constants_mod,              only: r_def, l_def, i_def
  use field_mod,                  only: field_type
  use testkern_three_scalars_mod, only: testkern_three_scalars_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a
  logical(l_def)   :: lswitch
  integer(i_def)   :: istep

  call invoke(                                                        &
       testkern_three_scalars_type(a, f1, f2, m1, m2, lswitch, istep) &
          )

end program single_invoke
