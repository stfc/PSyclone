! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module restrict_test_kernel_mod

  use constants_mod
  use kernel_mod
  use argument_mod

  implicit none

  private

  type, public, extends(kernel_type) :: restrict_test_kernel_type
     private
     type(arg_type) :: meta_args(2) = (/                            &
          arg_type(GH_FIELD, GH_REAL, GH_INC,  ANY_SPACE_1,         &
                                               mesh_arg=GH_COARSE), &
          arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2,         &
                                               mesh_arg=GH_FINE  )  &
          /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: restrict_test_kernel_code
  end type restrict_test_kernel_type

  public :: restrict_test_kernel_code

contains

  subroutine restrict_test_kernel_code(nlayers,                  &
                                       cell_map,                 &
                                       ncell_f_per_c_x,          &
                                       ncell_f_per_c_y, ncell_f, &
                                       coarse, fine,             &
                                       undf_aspc1, dofmap_aspc1, &
                                       ndf_aspc2, undf_aspc2, dofmap_aspc2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_f_per_c_x
    integer(kind=i_def), intent(in) :: ncell_f_per_c_y
    integer(kind=i_def), intent(in) :: ncell_f
    integer(kind=i_def), intent(in) :: ndf_aspc2
    integer(kind=i_def), intent(in) :: undf_aspc2, undf_aspc1
    integer(kind=i_def), dimension(ncell_f_per_c_x, ncell_f_per_c_y), &
                         intent(in) :: cell_map
    integer(kind=i_def), dimension(ndf_aspc2, ncell_f), intent(in) :: dofmap_aspc2
    integer(kind=i_def), dimension(ndf_aspc2), intent(in)          :: dofmap_aspc1
    real(kind=r_def), dimension(undf_aspc1), intent(inout) :: coarse
    real(kind=r_def), dimension(undf_aspc2), intent(in)    :: fine

  end subroutine restrict_test_kernel_code

end module restrict_test_kernel_mod
