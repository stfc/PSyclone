! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: invoke containing multiple kernels, the first and last
  ! two of which are a kernel that operates on the domain instead of a
  ! cell-column.
  use constants_mod,               only : r_def
  use testkern_domain_mod,         only: testkern_domain_type
  use testkern_mod,                only: testkern_type
  use testkern_anyd_any_space_mod, only: testkern_anyd_any_space_type
  use field_mod,                   only: field_type

  implicit none
  real(kind=r_def) :: b, c
  type(field_type) :: f1, f2, f3, f4, f5

  call invoke(                                           &
               ! Write to f1 (W3)                        
               testkern_domain_type(b, f1),              &
               ! Read from f1
               testkern_type(b, f2, f3, f4, f1),         &
               ! Read-write f1
               testkern_anyd_any_space_type(f1, f2, f3), &
               ! Write to f1 (W3)
               testkern_domain_type(c, f1),              &
               ! Another domain kernel but acting on a different field
               testkern_domain_type(c, f5) )

end program single_invoke
