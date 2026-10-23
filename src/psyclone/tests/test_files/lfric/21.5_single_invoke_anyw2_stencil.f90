! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_anyw2_stencil

  ! Description: test that correct code is produced when we have an
  ! any_w2 function space with a stencil in a kernel call
  use constants_mod,              only: i_def
  use field_mod,                  only: field_type
  use testkern_anyw2_stencil_mod, only: testkern_anyw2_stencil_type

  implicit none

  type(field_type) :: f1, f2, f3
  integer(i_def)   :: extent = 2
  
  call invoke(                                                 &
       testkern_anyw2_stencil_type(f1, f2, extent, f3, extent) &
          )

end program single_invoke_anyw2_stencil
