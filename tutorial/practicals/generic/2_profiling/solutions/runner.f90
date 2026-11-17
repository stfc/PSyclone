! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------
program runner
  use tra_adv_mod, only: tra_adv
  use profile_psy_data_mod, only: profile_psydatainit, profile_psydatashutdown

  call profile_psydatainit()

  call tra_adv()

  call profile_psydatashutdown()

end program runner
