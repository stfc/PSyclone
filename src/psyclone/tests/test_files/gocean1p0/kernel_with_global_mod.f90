! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2019-2024, Science and Technology Facilities Council.
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

module kernel_with_global_mod
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  private

  public time_smooth, time_smooth_code

  !> Constant declared in this module but then accessed within kernel - a
  !! problem for in-lining or other kernel transformations.
  real(go_wp) :: alpha
  
  type, extends(kernel_type) :: kernel_with_global
     type(go_arg), dimension(3) :: meta_args = &
          (/ go_arg(GO_READ,      GO_EVERY, GO_POINTWISE),     &
             go_arg(GO_READ,      GO_EVERY, GO_POINTWISE),     &
             go_arg(GO_READWRITE, GO_EVERY, GO_POINTWISE)      &
           /)

     integer :: iterates_over = GO_INTERNAL_PTS  
     integer :: index_offset = GO_OFFSET_ANY

  contains
    procedure, nopass :: code => kernel_with_global_code
  end type kernel_with_global

contains

  !> Kernel which accesses a variable declared in the parent module
  subroutine kernel_with_global_code(i, j, field, field_new, field_old)
    implicit none
    integer,  intent(in)                       :: i, j
    real(go_wp), intent(in),    dimension(:,:) :: field
    real(go_wp), intent(in),    dimension(:,:) :: field_new
    real(go_wp), intent(inout), dimension(:,:) :: field_old

    field_old(i,j) = field(i,j) + &
         alpha*(field_new(i,j) - 2.0d0*field(i,j) + field_old(i,j))

  end subroutine kernel_with_global_code

end module kernel_with_global_mod
