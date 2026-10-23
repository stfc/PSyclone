! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_any_space_2_mod

  use argument_mod
  use kernel_mod
  use constants_mod, only : r_def, i_def

  implicit none

  ! Test for any_space producing correct code where there are
  ! 1) multi declarations of the same any_space space,
  ! 2) no other spaces in the arguments,
  ! 3) no functions (e.g. basis, diff_basis) declared,
  ! 4) any_space used with an operator.
  type, public, extends(kernel_type) ::testkern_any_space_2_type
    private
    type(arg_type) :: meta_args(4) = (/                           &
         arg_type(GH_FIELD,    GH_REAL,    GH_INC,  ANY_SPACE_1), &
         arg_type(GH_FIELD,    GH_REAL,    GH_READ, ANY_SPACE_1), &
         arg_type(GH_OPERATOR, GH_REAL,    GH_READ, ANY_SPACE_1,  &
                                                    ANY_SPACE_1), &
         arg_type(GH_SCALAR,   GH_INTEGER, GH_READ)               &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, public, nopass :: testkern_any_space_2_code
  end type testkern_any_space_2_type

contains

  subroutine testkern_any_space_2_code(cell, nlayers, f1_data, f2_data, &
                                       ncell_3d, local_stencil, istep,  &
                                       ndf_aspc1, undf_aspc1, map_aspc1)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: undf_aspc1
    integer(kind=i_def), intent(in) :: istep
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in), dimension(ndf_aspc1) :: map_aspc1
    real(kind=r_def), intent(inout), dimension(undf_aspc1) :: f1_data
    real(kind=r_def), intent(in), dimension(undf_aspc1)    :: f2_data
    real(kind=r_def), intent(in), dimension(ncell_3d,ndf_aspc1,ndf_aspc1) :: local_stencil

  end subroutine testkern_any_space_2_code

end module testkern_any_space_2_mod
