! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program kernel_specific_example1

  ! The matrix vector_mm kernel currently requires additional
  ! boundary layer information to be set up which is not described
  ! in the API. Therefore, for the moment, we add this in when we
  ! see the matrix_vector_mm kernel.

  use inf,                      only: field_type, &
                                      operator_type
  use matrix_vector_kernel_mod, only: matrix_vector_kernel_type

  implicit none

  type(field_type)    :: f1, f2
  type(operator_type) :: f3

  call invoke(matrix_vector_kernel_type(f1, f2, f3))

end program kernel_specific_example1
