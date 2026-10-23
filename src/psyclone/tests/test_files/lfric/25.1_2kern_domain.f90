! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: invoke of multiple kernels, one of which operates on the
  ! domain instead of a cell-column.
  use constants_mod,       only : r_def
  use testkern_domain_mod, only: testkern_domain_type
  use testkern_mod,        only: testkern_type
  use field_mod,           only: field_type

  implicit none
  real(kind=r_def) :: b
  type(field_type) :: f1, f2, f3, f4

  call invoke( testkern_type(b, f2, f3, f4, f1), &
               testkern_domain_type(b, f1) )

end program single_invoke
