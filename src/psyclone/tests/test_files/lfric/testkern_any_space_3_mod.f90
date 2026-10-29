! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_any_space_3_mod

  use argument_mod
  use kernel_mod
  use constants_mod, only : r_def, i_def

  implicit none

  ! Test for any_space producing correct code where there are
  ! 1) different spaces for the to and from parts of an operator,
  ! 2) no other arguments.
  type, public, extends(kernel_type) ::testkern_any_space_3_type
    private
    type(arg_type) :: meta_args(1) = (/                  &
         arg_type(GH_OPERATOR, GH_REAL, GH_READWRITE,    &
                               ANY_SPACE_1, ANY_SPACE_2) &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, public, nopass :: testkern_any_space_3_code
  end type testkern_any_space_3_type

contains

  subroutine testkern_any_space_3_code(cell, nlayers,           &
                                       ncell_3d, local_stencil, &
                                       ndf_aspc1, ndf_aspc2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_aspc1, ndf_aspc2
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    real(kind=r_def), dimension(ncell_3d,ndf_aspc1,ndf_aspc2) :: local_stencil

  end subroutine testkern_any_space_3_code

end module testkern_any_space_3_mod
