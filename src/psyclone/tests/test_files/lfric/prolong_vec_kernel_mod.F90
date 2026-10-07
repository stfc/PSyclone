! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module prolong_vec_kernel_mod

  use constants_mod
  use argument_mod
  use kernel_mod
  use fs_continuity_mod

  implicit none

  private

  type, public, extends(kernel_type) :: prolong_vec_kernel_type
     private
     type(arg_type), dimension(2) :: meta_args = (/                      &
          arg_type(GH_FIELD*3, GH_REAL, GH_INC,  W1, mesh_arg=GH_FINE),  &
          arg_type(GH_FIELD*3, GH_REAL, GH_READ, W2, mesh_arg=GH_COARSE) &
          /)
     integer :: operates_on = CELL_COLUMN
   contains
     procedure, nopass :: code => prolong_vec_kernel_code
  end type prolong_vec_kernel_type

  public :: prolong_vec_kernel_code

contains

  subroutine prolong_vec_kernel_code(nlayers,                      &
                                     cell_map,                     &
                                     ncell_f_per_c_x,              &
                                     ncell_f_per_c_y,              &
                                     ncell_f,                      &
                                     fine_1, fine_2, fine_3,       &
                                     coarse_1, coarse_2, coarse_3, &
                                     ndf_w1, undf_w1, dofmap_w1,   &
                                     undf_w2, dofmap_w2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_f_per_c_x, ncell_f_per_c_y
    integer(kind=i_def), dimension(ncell_f_per_c_x, ncell_f_per_c_y), &
                         intent(in) :: cell_map
    integer(kind=i_def), intent(in) :: ncell_f
    integer(kind=i_def), intent(in) :: ndf_w1, undf_w1, undf_w2
    integer(kind=i_def), dimension(ndf_w1, ncell_f), intent(in) :: dofmap_w1
    integer(kind=i_def), dimension(ndf_w1), intent(in) :: dofmap_w2
    real(kind=r_def), dimension(undf_w1), intent(inout) :: fine_1, fine_2, fine_3
    real(kind=r_def), dimension(undf_w2), intent(in) :: coarse_1, coarse_2, coarse_3

  end subroutine prolong_vec_kernel_code

end module prolong_vec_kernel_mod
