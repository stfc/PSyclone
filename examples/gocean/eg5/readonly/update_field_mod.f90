! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
! Author: J. Henrichs, Bureau of Meteorology

!> This module contains a simple 'update' kernel that
!! overwrites a read-only declared field and a scalar
!! read-only value. This is done by computing the address
!! differences between an array and the value to be
!! overwritten, then using out-of-bounds array accesses
!! to change the values of parameters that are `intent(in)`.
!! Be warned, very hacky code to trigger the problem!
!! If array-bounds-checking is enabled these accesses will
!! be flagged.

module update_field_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod, only: GO_OFFSET_SW
  implicit none

  type, extends(kernel_type) :: update_field
     type(go_arg), dimension(8) :: meta_args =             &
          (/ go_arg(GO_READWRITE, GO_CT,       GO_POINTWISE),  & ! field
             go_arg(GO_READ,      GO_CT,       GO_POINTWISE),  & ! field
             go_arg(GO_READ,      GO_CT,       GO_POINTWISE),  & ! field
             go_arg(GO_READ,      GO_CT,       GO_POINTWISE),  & ! field
             go_arg(GO_READWRITE, GO_R_SCALAR, GO_POINTWISE),  & ! scalar
             go_arg(GO_WRITE,     GO_R_SCALAR, GO_POINTWISE),  & ! scalar
             go_arg(GO_READ,      GO_R_SCALAR, GO_POINTWISE),  & ! scalar
             go_Arg(GO_READ,      GO_GRID_DX_CONST)                  &
           /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
     integer :: ITERATES_OVER = GO_ALL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => update_field_code
  end type update_field

contains

  subroutine update_field_code(i, j, a_f, b_f, c_f, d_f, x, y, z, grid_dx)
    integer, intent(in) :: i, j
    real(go_wp), dimension(:,:), intent(inout) :: a_f
    real(go_wp), dimension(:,:), intent(in) :: b_f, c_f, d_f
    real(go_wp), intent(inout) :: x
    real(go_wp), intent(out)   :: y
    real(go_wp), intent(in)    :: z, grid_dx
    integer(kind=8) :: offset

    ! VERY NAUGHTY CODE AHEAD - there be dragon!!
    ! We modify the values of some read-only fields by using offsets
    ! in a write-able field. This will abort if the code is compiled
    ! with array bound check of course.

    ! First, modify z:
    offset = ( loc(z) - loc(a_f(1,1)) ) / sizeof(z)
    a_f(1+offset, 1) = 123.0

    ! Then modify b(1,1)
    offset = ( loc(b_f(1,1)) - loc(a_f(1,1)) ) / sizeof(z)
    a_f(1+offset, 1) = 123.0

  end subroutine update_field_code

end module update_field_mod
