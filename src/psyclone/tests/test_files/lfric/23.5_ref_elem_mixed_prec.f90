! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Description: Declare field data as r_solver to check that r_def is
! declared for the reference element arrays in the psy-layer.

program ref_element_mixed_precision

  use r_solver_field_mod,       only: r_solver_field_type
  use testkern_ref_elem_mp_mod, only: testkern_ref_elem_mp_type

  implicit none

  type(r_solver_field_type) :: f1

  call invoke( testkern_ref_elem_mp_type(f1) )

end program ref_element_mixed_precision
