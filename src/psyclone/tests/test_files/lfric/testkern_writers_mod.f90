! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

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
