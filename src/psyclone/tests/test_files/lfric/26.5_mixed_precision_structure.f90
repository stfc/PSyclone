! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module self_symbols_structure

  use constants_mod, only : r_solver
  use r_solver_field_mod, only : r_solver_field_type
  use r_solver_operator_mod, only : r_solver_operator_type
  use quadrature_xyoz_mod, only : quadrature_xyoz_type
  use testkern_operator_mod, only : testkern_operator_type
  use bundle_type_mod, only : bundletype

  type :: my_type
     type(quadrature_xyoz_type), pointer :: qr => null
   contains
     procedure, public :: my_sub
  end type my_type

  type(bundletype) :: bundle

  type :: bundle2type
     real(r_def) :: a(3)
  end type bundle2type

  type :: bundle3type
     type(r_solver_field_type) :: coord(3)
     type(bundle2type) :: x(2)
  end type bundle3type

  type(bundle2type) :: bundle2
  type(bundle3type) :: bundle3

contains

  subroutine my_sub(self)
    class (my_type), intent(in) :: self
    call invoke(testkern_operator_type(bundle%stuff(1)%b%mm_w0(1), bundle3%coord, bundle2%x(1)%a(0), self%qr))
  end subroutine my_sub

end module self_symbols_structure
