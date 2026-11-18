! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multi_kern_nlayers_ndata

  ! Description: invoke of multiple kernels which have field arguments with
  ! non-default values for number of vertical layers and number of data
  ! elements per dof.
  use constants_mod,              only: r_def
  use field_mod,                  only: field_type
  use testkern_nlayers_ndata_mod, only: testkern_nlayers_ndata_type
  use testkern_nlayers_ndata_anydspace_mod, only: testkern_nlayers_ndata_anydspace_type
  implicit none
  real(kind=r_def) :: a
  type(field_type) :: f1, f2, f3, f4, f5, f6
  type(field_type) :: df7, df8, df9

  call invoke( testkern_nlayers_ndata_type(a, f1, f2, f3, f4, f5, f6), &
               testkern_nlayers_ndata_anydspace_type(a, f1, df7, df8), &
               ! Reorder arguments that are on ANY_DISCONTINUOUS_SPACE_*
               testkern_nlayers_ndata_anydspace_type(a, f1, df8, df9) )

end program multi_kern_nlayers_ndata
