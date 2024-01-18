!-------------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2021-2024, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author: R. W. Ford STFC Daresbury Lab
!
! Example with an undeclared symbol that could be provided via a
! wildcard module (wildcard_mod).

module self_symbols_use

  use constants_mod, only : r_solver
  use r_solver_field_mod, only : r_solver_field_type
  use r_solver_operator_mod, only : r_solver_operator_type
  use quadrature_xyoz_mod, only : quadrature_xyoz_type
  use testkern_operator_mod, only : testkern_operator_type

  use some_mod, only : a
  use wildcard_mod

  type :: my_type
     type(r_solver_field_type) :: coord(3)
     type(quadrature_xyoz_type), pointer :: qr => null
   contains
     procedure, public :: my_sub
  end type my_type

contains

  subroutine my_sub(self)
    class (my_type), intent(in) :: self
    call invoke(testkern_operator_type(mm_w0, self%coord, a, self%qr))
  end subroutine my_sub

end module self_symbols_use
