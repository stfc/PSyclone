! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
! Author: A. R. Porter, STFC Daresbury Lab

module time_smooth_mod
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  IMPLICIT none

  PRIVATE

  PUBLIC time_smooth, time_smooth_code

  !> The time smoothing operates in time rather than space
  !! and therefore takes three fields defined on any one
  !! of the four grid point types (T, U, V or Q).
  !! Presumably FE should be FD for us and maybe CELLS 
  !! should be COLUMNS?
  TYPE, EXTENDS(kernel_type) :: time_smooth
     TYPE(go_arg), DIMENSION(3) :: meta_args = &
          (/ go_arg(GO_READ,      GO_EVERY, GO_POINTWISE),     &
             go_arg(GO_READ,      GO_EVERY, GO_POINTWISE),     &
             go_arg(GO_READWRITE, GO_EVERY, GO_POINTWISE)      &
           /)

     !> This kernel writes only to internal points of the
     !! simulation domain.
     INTEGER :: ITERATES_OVER = GO_INTERNAL_PTS
  
     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel is independent of this choice (because it
     !! acts in time rather than space).
     integer :: index_offset = GO_OFFSET_ANY

  CONTAINS
    procedure, nopass :: code => time_smooth_code
  END type time_smooth

CONTAINS

  !> Kernel to smooth supplied field in time
  SUBROUTINE time_smooth_code(i, j, field, field_new, field_old)
    IMPLICIT none
    INTEGER,  INTENT(in)                    :: i, j
    REAL(go_wp), INTENT(in),    DIMENSION(:,:) :: field
    REAL(go_wp), INTENT(in),    DIMENSION(:,:) :: field_new
    REAL(go_wp), INTENT(inout), DIMENSION(:,:) :: field_old
    REAL(go_wp) :: alpha
    ! 'alpha' was originally a module variable but that causes problems
    ! when transforming kernels so it has been made local to keep
    ! existing tests happy. There are separate test kernels for
    ! exercising this aspect of PSyclone (e.g. kernel_with_use_mod.f90).
    alpha = 1.0d0

    field_old(i,j) = field(i,j) + &
         alpha*(field_new(i,j) - 2.0d0*field(i,j) + field_old(i,j))

  END SUBROUTINE time_smooth_code

END MODULE time_smooth_mod
