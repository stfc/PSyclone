! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2024, Science and Technology Facilities Council
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
! Author R. W. Ford, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

module testkern_writers_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  ! Test halo_dirty calls only for field "writers", that is write, readwrite
  ! and inc fields (not for read)
  type, extends(kernel_type) :: testkern_writers_type
     type(arg_type) :: meta_args(8) = (/                 &
          arg_type(GH_FIELD, GH_REAL, GH_WRITE,     W3), &
          arg_type(GH_FIELD, GH_REAL, GH_READ,      W1), &
          arg_type(GH_FIELD, GH_REAL, GH_INC,       W1), &
          arg_type(GH_FIELD, GH_REAL, GH_READ,      W1), &
          arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W3), &
          arg_type(GH_FIELD, GH_REAL, GH_WRITE,     W3), &
          arg_type(GH_FIELD, GH_REAL, GH_INC,       W1), &
          arg_type(GH_FIELD, GH_REAL, GH_INC,       W1)  &
          /)
     integer :: operates_on = CELL_COLUMN
   contains
     procedure, public, nopass :: testkern_writers_code
  end type testkern_writers_type

contains

  subroutine testkern_writers_code(nlayers, fld1, fld2,     &
                                   fld3, fld4, fld5,        &
                                   fld6, fld7, fld8,        &
                                   ndf_w3, undf_w3, map_w3, &
                                   ndf_w1, undf_w1, map_w1)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3, undf_w1
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def), intent(inout), dimension(undf_w3) :: fld1
    real(kind=r_def), intent(in),    dimension(undf_w1) :: fld2
    real(kind=r_def), intent(inout), dimension(undf_w1) :: fld3
    real(kind=r_def), intent(in),    dimension(undf_w1) :: fld4
    real(kind=r_def), intent(inout), dimension(undf_w3) :: fld5
    real(kind=r_def), intent(inout), dimension(undf_w3) :: fld6
    real(kind=r_def), intent(inout), dimension(undf_w1) :: fld7
    real(kind=r_def), intent(inout), dimension(undf_w1) :: fld8

  end subroutine testkern_writers_code

end module testkern_writers_mod
