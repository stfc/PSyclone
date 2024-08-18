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
! Author: A. R. Porter, STFC Daresbury Lab.

module kern_nested_use_mod
  implicit none

  type, extends(kernel_type) :: kern_nested_use
     type(arg), dimension(1) :: meta_args =              &
          (/ go_arg(GO_READWRITE, GO_CT, GO_POINTWISE) /)
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => kern_nested_use_code
  end type kern_nested_use

contains

  subroutine kern_nested_use_code(i, j, fld)
    use another_mod, only: another_kern
    integer, intent(in) :: i, j
    real(go_wp), dimension(:,:), intent(inout) :: fld

    call another_kern(i, j, fld)

  end subroutine kern_nested_use_code

end module kern_nested_use_mod
