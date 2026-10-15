! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single function specified in an invoke call where the
  ! quadrature is supplied by dereferencing a derived type. Note, none of
  ! the actual LFRic objects has quadrature so we use a fake reference-
  ! element-like object that contains a quadrature object.
  use constants_mod,         only: r_def, i_def
  use reference_element_mod, only: reference_element_type
  use field_mod,             only: field_type
  use testkern_qr_mod,       only: testkern_qr_type

  implicit none

  type(field_type)             :: f1, f2, m1, m2
  type(reference_element_type) :: unit_cube
  real(r_def)                  :: a
  integer(i_def)               :: istp

  call invoke( testkern_qr_type(f1, f2, m1, a, m2, istp, unit_cube%qr_XYoZ) )

end program single_invoke
