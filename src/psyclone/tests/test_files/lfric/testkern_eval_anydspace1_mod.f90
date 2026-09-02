! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_eval_anydspace1_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_eval_anydspace1_type
     type(arg_type)  :: meta_args(3) = (/                                   &
          arg_type(GH_FIELD, GH_REAL, GH_WRITE, ANY_DISCONTINUOUS_SPACE_1), &
          arg_type(GH_FIELD, GH_REAL, GH_READ,  W0),                        &
          arg_type(GH_FIELD, GH_REAL, GH_READ,  W1)                         &
          /)
     type(func_type) :: meta_funcs(2) = (/                                  &
          func_type(W0, GH_BASIS),                                          &
          func_type(W1, GH_DIFF_BASIS)                                      &
          /)
     integer :: operates_on = CELL_COLUMN
     integer :: gh_shape = gh_evaluator
   contains
     procedure, nopass :: code => testkern_eval_anydspace1_code
  end type testkern_eval_anydspace1_type

contains

  subroutine testkern_eval_anydspace1_code(nlayers,                 &
                                           field1, field2, field3,  &
                                           ndf_adspc1, undf_adspc1, &
                                           map_adspc1,              &
                                           ndf_w0, undf_w0, map_w0, &
                                           basis_w0_on_adspc1,      &
                                           ndf_w1, undf_w1, map_w1, &
                                           diff_basis_w1_on_adspc1)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_adspc1
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: undf_adspc1, undf_w0, undf_w1
    integer(kind=i_def), intent(in), dimension(ndf_adspc1) :: map_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_w0)     :: map_w0
    integer(kind=i_def), intent(in), dimension(ndf_w1)     :: map_w1
    real(kind=r_def), intent(inout), dimension(undf_adspc1) :: field1
    real(kind=r_def), intent(in), dimension(undf_w0)        :: field2
    real(kind=r_def), intent(in), dimension(undf_w1)        :: field3
    real(kind=r_def), intent(in), dimension(1,ndf_w0,ndf_adspc1) :: basis_w0_on_adspc1
    real(kind=r_def), intent(in), dimension(3,ndf_w1,ndf_adspc1) :: diff_basis_w1_on_adspc1

  end subroutine testkern_eval_anydspace1_code

end module testkern_eval_anydspace1_mod
