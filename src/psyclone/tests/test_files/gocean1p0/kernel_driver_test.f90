! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2019-2024, Science and Technology Facilities Council
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
! Author J. Henrichs, Bureau of Meteorology
! -----------------------------------------------------------------------------

!> \brief Simple test case for extract driver generation.
!! \detail This kernel has all type of variable accesses:
!! input, input+output, output

module kernel_driver
  use argument_mod
  use field_mod
  use grid_mod
  use kind_params_mod
  use kernel_mod
  implicit none

  private

  public compute_kernel_code

  type, extends(kernel_type) :: compute_kernel
     type(go_arg), dimension(6) :: meta_args =    &
          (/ go_arg(GO_WRITE,     GO_CU, GO_POINTWISE),   &
             go_arg(GO_READWRITE, GO_CT, GO_POINTWISE),   &
             go_arg(GO_READ,      GO_CU, GO_POINTWISE),   &
             go_arg(GO_READ,      GO_CU, GO_POINTWISE),   &
             go_Arg(GO_READ,      GO_GRID_DX_CONST),      &
             go_Arg(GO_READ,      GO_GRID_LAT_U)          &
           /)
     integer :: ITERATES_OVER = GO_INTERNAL_PTS
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_kernel_code
  end type compute_kernel

contains

  !===================================================
  subroutine compute_kernel_code(i, j, out_f, inout_f, in_f, &
                                 dummy_field, dx_const, lat_u)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(out),   dimension(:,:) :: out_f
    real(go_wp), intent(inout), dimension(:,:) :: inout_f
    real(go_wp), intent(in),    dimension(:,:) :: in_f
    real(go_wp), intent(in),    dimension(:,:) :: dummy_field
    real(go_wp), intent(in)                    :: dx_const
    real(go_wp), intent(in),    dimension(:,:) :: lat_u

    out_f(i,j) = in_f(i,j) + dx_const * lat_u(i,j)
    inout_f(i,j) = inout_f(i,j) + in_f(i,j)*dummy_field(i,j)

  end subroutine compute_kernel_code

end module kernel_driver
