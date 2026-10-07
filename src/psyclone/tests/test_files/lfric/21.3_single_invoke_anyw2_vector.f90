! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_anyw2_vector

  ! Description: test that correct code is produced when we have an
  ! any_w2 function space with a vector field in a kernel call
  use field_mod,                 only: field_type
  use quadrature_xyoz_mod,       only: quadrature_xyoz_type
  use testkern_anyw2_vector_mod, only: testkern_anyw2_vector_type

  implicit none

  type(field_type)           :: f1, f2, f3(2)
  type(quadrature_xyoz_type) :: qr
  
  call invoke(                                    &
       testkern_anyw2_vector_type(f1, f2, f3, qr) &
          )

end program single_invoke_anyw2_vector
