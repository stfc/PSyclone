! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2023, Science and Technology Facilities Council.
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
! Author: R. W. Ford, STFC Daresbury Laboratory
! Modified: I. Kavcic, Met Office

! An example of a kernel that provides an interface to different
! kernel implementations which support different precisions for
! scientifically-defined precisions of fields and scalar arguments.
! In this example 32 and 64 bit implementations are provided.

module mixed_sci_kernel_mod

  use argument_mod,      only : arg_type,                 &
                                GH_FIELD, GH_SCALAR,      &
                                GH_REAL, GH_READ, GH_INC, &
                                GH_READWRITE, CELL_COLUMN
  use fs_continuity_mod, only : WTHETA, W2
  use constants_mod,     only : r_def, i_def
  use kernel_mod,        only : kernel_type

  implicit none

  type, extends(kernel_type) :: mixed_sci_kernel_type
     type(arg_type), dimension(4) :: meta_args =                &
          (/ arg_type(GH_SCALAR, GH_REAL, GH_READ),             &
             arg_type(GH_FIELD,  GH_REAL, GH_INC,       W2),    &
             arg_type(GH_SCALAR, GH_REAL, GH_READ),             &
             arg_type(GH_FIELD,  GH_REAL, GH_READWRITE, WTHETA) &
          /)
     integer :: operates_on = cell_column
  end type mixed_sci_kernel_type

  private
  public :: mixed_sci_code

  interface mixed_sci_code
     module procedure mixed_sci_code_32
     module procedure mixed_sci_code_64
  end interface mixed_sci_code

contains

  subroutine mixed_sci_code_32(nlayers, rscalar_1, field_w2, &
                               rscalar_2, field_wtheta,      &
                               ndf_w2, undf_w2, map_w2,      &
                               ndf_wtheta, undf_wtheta, map_wtheta)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in), dimension(ndf_wtheta) :: map_wtheta
    integer(kind=i_def), intent(in) :: undf_wtheta, undf_w2
    real*4, intent(in) :: rscalar_1, rscalar_2
    real*4, intent(inout), dimension(undf_w2) :: field_w2
    real*4, intent(inout), dimension(undf_wtheta) :: field_wtheta

    write(*,*) "32-bit example called"

  end subroutine mixed_sci_code_32

  subroutine mixed_sci_code_64(nlayers, rscalar_1, field_w2, &
                               rscalar_2, field_wtheta,      &
                               ndf_w2, undf_w2, map_w2,      &
                               ndf_wtheta, undf_wtheta, map_wtheta)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in), dimension(ndf_wtheta) :: map_wtheta
    integer(kind=i_def), intent(in) :: undf_wtheta, undf_w2
    real*8, intent(in) :: rscalar_1, rscalar_2
    real*8, intent(inout), dimension(undf_w2) :: field_w2
    real*8, intent(inout), dimension(undf_wtheta) :: field_wtheta

    write(*,*) "64-bit example called"

  end subroutine mixed_sci_code_64

end module mixed_sci_kernel_mod
