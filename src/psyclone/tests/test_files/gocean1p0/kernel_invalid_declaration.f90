! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2022-2024, Science and Technology Facilities Council
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
! Author R. W. Ford, STFC Daresbury Lab

! An example GOcean kernel that is invalid as the variable U is not
! declared in the subroutine.

module kernel_invalid_fortran
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use field_mod
  use grid_mod
  implicit none

  private

  public compute, compute_code

  type, extends(kernel_type) :: compute
     type(go_arg), dimension(3) :: meta_args =       &
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),  & ! cu
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),  & ! p
             go_arg(GO_READ,  GO_CU, GO_POINTWISE)   & ! u
           /)
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_code
  end type compute

contains

  !===================================================

  ! u is not declared
  subroutine compute_code(i, j, cu, p, u)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(out), dimension(:,:) :: cu
    real(go_wp), intent(in),  dimension(:,:) :: p

    CU(I,J) = 0.5d0*(P(i+1,J)+P(I,J))*U(I,J)

  end subroutine compute_code

end module kernel_invalid_fortran
