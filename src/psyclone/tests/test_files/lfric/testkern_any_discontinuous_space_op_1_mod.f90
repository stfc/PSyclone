! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_any_discontinuous_space_op_1_mod

  use constants_mod
  use argument_mod
  use kernel_mod

  implicit none

  ! Description: test for any_discontinuous_space producing correct code
  ! when there are
  ! 1) multiple declarations of the same any_discontinuous_space,
  ! 2) other any_discontinuous_space spaces in the arguments,
  ! 3) no functions (e.g. basis, diff_basis) declared,
  ! 4) any_discontinuous_space used with an operator,
  ! 5) different to- and from- any_discontinuous_space spaces used with
  !    an operator.

  type, public, extends(kernel_type) :: testkern_any_discontinuous_space_op_1_type
    private
    type(arg_type) :: meta_args(5) = (/                                           &
         arg_type(GH_FIELD*3,  GH_REAL, GH_READ,      ANY_DISCONTINUOUS_SPACE_1), &
         arg_type(GH_FIELD,    GH_REAL, GH_READWRITE, ANY_DISCONTINUOUS_SPACE_2), &
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,      ANY_DISCONTINUOUS_SPACE_1,  &
                                                      ANY_DISCONTINUOUS_SPACE_1), &
         arg_type(GH_OPERATOR, GH_REAL, GH_WRITE,     ANY_DISCONTINUOUS_SPACE_3,  &
                                                      ANY_DISCONTINUOUS_SPACE_7), &
         arg_type(GH_SCALAR,   GH_REAL, GH_READ)                                  &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, public, nopass :: testkern_any_discontinuous_space_op_1_code
  end type testkern_any_discontinuous_space_op_1_type

contains

  subroutine testkern_any_discontinuous_space_op_1_code(              &
                                        cell, nlayers,                &
                                        field1_x, field1_y, field1_z, &
                                        field2,                       &
                                        ncell_3d_op3, op3,            &
                                        ncell_3d_op4, op4,            &
                                        rscalar,                      &
                                        ndf1, undf1, map1,            &
                                        ndf2, undf2, map2,            &
                                        ndf_to_op4, ndf_from_op4)

      implicit none

      integer(kind=i_def), intent(in) :: nlayers
      integer(kind=i_def), intent(in) :: ndf1, ndf2
      integer(kind=i_def), intent(in) :: undf1, undf2
      integer(kind=i_def), intent(in) :: ndf_to_op4, ndf_from_op4
      integer(kind=i_def), intent(in) :: cell
      integer(kind=i_def), intent(in) :: ncell_3d_op3
      integer(kind=i_def), intent(in) :: ncell_3d_op4
      integer(kind=i_def), intent(in), dimension(ndf1) :: map1
      integer(kind=i_def), intent(in), dimension(ndf2) :: map2
      real(kind=r_def), intent(in) :: rscalar
      real(kind=r_def), intent(in), dimension(undf1)    :: field1_x, field1_y, &
                                                           field1_z
      real(kind=r_def), intent(inout), dimension(undf2) :: field2
      real(kind=r_def), intent(in), dimension(ncell_3d_op3,ndf1,ndf1)                  :: op3
      real(kind=r_def), intent(inout), dimension(ncell_3d_op4,ndf_to_op4,ndf_from_op4) :: op4

  end subroutine testkern_any_discontinuous_space_op_1_code

end module testkern_any_discontinuous_space_op_1_mod
