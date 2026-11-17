! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------
!> A bare-bones driver program that allows the generated code to be compiled
!! and linked. The result will not execute because no attempt has been made
!! to perform the setup that a real LFRic application requires.
program driver
  USE mesh_mod, ONLY: mesh_type
  use test_alg_mod, only: test_alg
  implicit none
  TYPE(mesh_type), POINTER :: mesh

  call test_alg(mesh)

end program driver
