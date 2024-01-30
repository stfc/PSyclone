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
! Author: R. W. Ford, STFC Daresbury Lab
! Modified: I. Kavcic, Met Office
!           A. R. Porter, STFC Daresbury Lab

! A kernel which writes to a field on 'W2' and has a stencil access. Such a kernel
! must guarantee that any writes to a given shared entity are for the same value
! and that the first access to such an entity is a write.
module testkern_write_w2_stencil_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, public, extends(kernel_type) :: testkern_write_w2_stencil_type
    private
    type(arg_type) :: meta_args(4) = (/                                 &
         arg_type(GH_FIELD,   GH_REAL, GH_WRITE,  W2),                  &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,   W2, STENCIL(CROSS)),  &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,   W2, STENCIL(CROSS)),  &
         arg_type(GH_SCALAR,  GH_REAL, GH_READ)                         &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: testkern_write_w2_stencil_code
  end type

contains

subroutine testkern_write_w2_stencil_code(nlayers,                               &
                                          u_inc, u_n,                            &
                                          map_w2_size, map_w2,                   &
                                          dx_at_w2,                              &
                                          map_dx_stencil_size, map_dx_stencil,   &
                                          viscosity_mu,                          &
                                          ndf_w2, undf_w2, cell_map_w2)

  implicit none

  integer(kind=i_def), intent(in) :: nlayers
  integer(kind=i_def), intent(in) :: ndf_w2, undf_w2
  integer(kind=i_def), intent(in) :: map_w2_size, map_dx_stencil_size
  integer(kind=i_def), dimension(ndf_w2,map_w2_size), intent(in)  :: map_w2
  integer(kind=i_def), dimension(ndf_w2,map_dx_stencil_size), intent(in)  :: map_dx_stencil
  integer(kind=i_def), dimension(ndf_w2),                     intent(in)  :: cell_map_w2

  real(kind=r_def), dimension(undf_w2),  intent(inout) :: u_inc
  real(kind=r_def), dimension(undf_w2),  intent(in)    :: u_n, dx_at_w2

  real(kind=r_def), intent(in) :: viscosity_mu

end subroutine testkern_write_w2_stencil_code

end module testkern_write_w2_stencil_mod
