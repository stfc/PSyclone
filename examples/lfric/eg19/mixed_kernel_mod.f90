! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! An example of a kernel that provides an interface to different
! kernel implementations which support different precision (of the
! real operator, field and scalar arguments). In this example 32 and
! 64 bit implementations are provided.

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
          (/ arg_type(GH_SCALAR,   GH_REAL, GH_READ),          &
             arg_type(GH_FIELD,    GH_REAL, GH_READWRITE, W3), &
             arg_type(GH_OPERATOR, GH_REAL, GH_READ, W0, W0)   &
          /)
     integer :: operates_on = cell_column
  end type mixed_kernel_type

  private
  public :: mixed_code

  interface mixed_code
     module procedure mixed_code_32
     module procedure mixed_code_64
  end interface mixed_code

contains

  subroutine mixed_code_32(cell, nlayers, rscalar,    &
                           field_w3, op_ncell_3d, op, &
                           ndf_w3, undf_w3, map_w3, ndf_w0)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    integer(kind=i_def), intent(in) :: undf_w3, ndf_w0
    real*4,              intent(in) :: rscalar
    real*4,              intent(inout), dimension(undf_w3) :: field_w3
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: op_ncell_3d
    real*4,              intent(in), dimension(op_ncell_3d,ndf_w0,ndf_w0) :: op

    write(*,*) "32-bit example called"

  end subroutine mixed_code_32

  subroutine mixed_code_64(cell, nlayers, rscalar,    &
                           field_w3, op_ncell_3d, op, &
                           ndf_w3, undf_w3, map_w3, ndf_w0)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    integer(kind=i_def), intent(in) :: undf_w3, ndf_w0
    real*8,              intent(in) :: rscalar
    real*8,              intent(inout), dimension(undf_w3) :: field_w3
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: op_ncell_3d
    real*8,              intent(in), dimension(op_ncell_3d,ndf_w0,ndf_w0) :: op

    write(*,*) "64-bit example called"

  end subroutine mixed_code_64

end module mixed_kernel_mod
