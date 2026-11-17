! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module self_example

  use constants_mod, only : r_solver
  use r_solver_field_mod, only : r_solver_field_type
  use r_solver_operator_mod, only : r_solver_operator_type
  use quadrature_xyoz_mod, only : quadrature_xyoz_type
  use testkern_operator_real_mod, only : testkern_operator_type

  type :: my_type
     type(r_solver_field_type) :: coord(3)
     type(quadrature_xyoz_type), pointer :: qr => null
     type(r_solver_operator_type) :: mm_w0
     real(kind=r_solver) :: a
   contains
     procedure, public :: my_sub
  end type my_type

contains

  subroutine my_sub(self)
    class (my_type), intent(in) :: self
    call invoke(testkern_operator_type(self%mm_w0, self%coord, self%a, self%qr))
  end subroutine my_sub

end module self_example
