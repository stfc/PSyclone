! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Invokes four kernels, three of which include the halo in their iteration
  ! space. The depth to which the halo is accessed is specified by a mixture
  ! of literals and variables.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_mod,  only: testkern_type
  use testkern_halo_only_mod,  only: testkern_halo_only_type
  use testkern_halo_and_owned_mod, only: testkern_halo_and_owned_type
  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a
  integer :: hdepth_depth

  call invoke( testkern_halo_only_type(a, f1, f2, m1, m2, 2), &
               testkern_type(a, f1, f2, m1, m2), &
               testkern_halo_and_owned_type(a, f1, f2, m1, m2, hdepth), &
               testkern_halo_and_owned_type(a, f1, f2, m1, m2, 5))

end program single_invoke
