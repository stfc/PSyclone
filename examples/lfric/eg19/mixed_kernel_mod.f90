! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2022, Science and Technology Facilities Council.
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

! Example mixed precision kernel supporting 32- and 64- bit
! implementations.

module mixed_kernel_mod

  use argument_mod,      only : arg_type,                         &
                                GH_FIELD, GH_OPERATOR, GH_SCALAR, &
                                GH_REAL, GH_READ, GH_READWRITE,   &
                                CELL_COLUMN
  use fs_continuity_mod, only : W3, W0
  use constants_mod,     only : r_def, i_def
  use kernel_mod,        only : kernel_type

  implicit none

  type, extends(kernel_type) :: mixed_kernel_type
     type(arg_type), dimension(3) :: meta_args =               &
          (/ arg_type(gh_scalar,   gh_real, gh_read),          &
             arg_type(gh_field,    gh_real, gh_readwrite, w3), &
             arg_type(gh_operator, gh_real, gh_read, w0, w0)   &
          /)
     integer :: operates_on = cell_column
  end type mixed_kernel_type

  private
  public :: mixed_kernel_code

  interface mixed_kernel_code
     module procedure mixed_kernel_code_32
     module procedure mixed_kernel_code_64
  end interface mixed_kernel_code

contains

  subroutine mixed_kernel_code_32(cell, nlayers, rscalar, field_w3, op_ncell_3d, op,  &
                           ndf_w3, undf_w3, map_w3, ndf_w0)
    USE constants_mod, ONLY: i_def
    IMPLICIT NONE
    INTEGER(KIND=i_def), intent(in) :: nlayers
    INTEGER(KIND=i_def), intent(in) :: ndf_w3
    INTEGER(KIND=i_def), intent(in), dimension(ndf_w3) :: map_w3
    INTEGER(KIND=i_def), intent(in) :: undf_w3, ndf_w0
    REAL*4,              intent(in) :: rscalar
    REAL*4,              intent(inout), dimension(undf_w3) :: field_w3
    INTEGER(KIND=i_def), intent(in) :: cell
    INTEGER(KIND=i_def), intent(in) :: op_ncell_3d
    REAL*4,              intent(in), dimension(ndf_w0,ndf_w0,op_ncell_3d) :: op
    print *, "32-bit example called"
  end subroutine mixed_kernel_code_32

  subroutine mixed_kernel_code_64(cell, nlayers, rscalar, field_w3, op_ncell_3d, op,  &
                           ndf_w3, undf_w3, map_w3, ndf_w0)
    USE constants_mod, ONLY: i_def
    IMPLICIT NONE
    INTEGER(KIND=i_def), intent(in) :: nlayers
    INTEGER(KIND=i_def), intent(in) :: ndf_w3
    INTEGER(KIND=i_def), intent(in), dimension(ndf_w3) :: map_w3
    INTEGER(KIND=i_def), intent(in) :: undf_w3, ndf_w0
    REAL*8,              intent(in) :: rscalar
    REAL*8,              intent(inout), dimension(undf_w3) :: field_w3
    INTEGER(KIND=i_def), intent(in) :: cell
    INTEGER(KIND=i_def), intent(in) :: op_ncell_3d
    REAL*8,              intent(in), dimension(ndf_w0,ndf_w0,op_ncell_3d) :: op
    print *, "64-bit example called"
  end subroutine mixed_kernel_code_64

end module mixed_kernel_mod
