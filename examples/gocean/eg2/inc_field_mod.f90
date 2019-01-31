! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2018-2019, Science and Technology Facilities Council.
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

module inc_field_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod, only: GO_OFFSET_SW
  implicit none

  type, extends(kernel_type) :: inc_field
     type(go_arg), dimension(4) :: meta_args =             &
          (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),        & ! field
             ! We have to pass in the extend of the field array as PGI
             ! does not support assumed-size arguments in accelerator
             ! regions. Ultimately PSyclone will do this for us.
             go_arg(GO_READ,  GO_I_SCALAR, GO_POINTWISE),  & ! nx
             go_arg(GO_READ,  GO_I_SCALAR, GO_POINTWISE),  & ! ny
             go_arg(GO_READ,  GO_I_SCALAR, GO_POINTWISE)   & ! istp
           /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
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
    procedure, nopass :: code => inc_field_code
  end type inc_field

contains

  subroutine inc_field_code(ji, jj, fld1, nx, ny, istp)
    integer, intent(in) :: ji, jj, nx, ny
    real(go_wp), dimension(nx,ny), intent(inout) :: fld1
    integer, intent(in) :: istp

    fld1(ji,jj) = fld1(ji,jj) + real(istp, go_wp)
    
  end subroutine inc_field_code

end module inc_field_mod
