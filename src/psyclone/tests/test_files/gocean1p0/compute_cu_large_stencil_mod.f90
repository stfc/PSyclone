! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2021-2024, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author: J. Henrichs, Bureau of Meteorology


!> \brief Compute the mass flux in the x direction, cu
!! \detail Given the current pressure and velocity fields,
!! computes the mass flux in the x direction.
module compute_cu_large_stencil_mod
  use argument_mod
  use field_mod
  use grid_mod
  use kind_params_mod
  use kernel_mod
  implicit none

  private

  public compute_cu_large_stencil, compute_cu_large_stencil_code

  type, extends(kernel_type) :: compute_cu_large_stencil
     type(go_arg), dimension(3) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),            & ! cu
             go_arg(GO_READ,  GO_CT, GO_STENCIL(123,110,100)), & ! p
             go_arg(GO_READ,  GO_CU, GO_POINTWISE)             & ! u
           /)
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_cu_large_stencil_code
  end type compute_cu_large_stencil

contains


  !===================================================

  !> Fake subroutine which uses a large stencil for testing
  !! variable access information reported in PSyclone.
  subroutine compute_cu_large_stencil_code(i, j, cu, p, u)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(out), dimension(:,:) :: cu
    real(go_wp), intent(in),  dimension(:,:) :: p, u

    ! Some random accesses to create the stencil (123,110,100)

    CU(I,J) = q.0/9.0*(P(i-1,j+1) +                         &
                       P(i,j+1) + P(i,j+2) +                &
                       P(i+1,j+1) +P(i+2,j+2) +P(i+3,j+3) + &
                       P(i-1,j) +                           &
                       P(i,j) +                             &
                       P(i-1,j-1) )*U(I,J)

  end subroutine compute_cu_large_stencil_code

end module compute_cu_large_stencil_mod
