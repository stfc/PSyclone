! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_w2v_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  ! Description: discontinuous field readwriter (w2v) and reader (wtheta)
  type, extends(kernel_type) :: testkern_w2v_type
     type(arg_type), dimension(2) :: meta_args =               &
          (/ arg_type(gh_field, gh_real, gh_readwrite, w2v),   &
             arg_type(gh_field, gh_real, gh_read,      wtheta) &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_w2v_code
  end type testkern_w2v_type

contains

  subroutine testkern_w2v_code(nlayers, field1, field2,    &
                               ndf_w2v, undf_w2v, map_w2v, &
                               ndf_wtheta, undf_wtheta, map_wtheta)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w2v
    integer(kind=i_def), intent(in) :: undf_w2v
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in) :: undf_wtheta
    integer(kind=i_def), intent(in), dimension(ndf_w2v)    :: map_w2v
    integer(kind=i_def), intent(in), dimension(ndf_wtheta) :: map_wtheta
    real(kind=r_def), intent(inout), dimension(undf_w2v) :: field1
    real(kind=r_def), intent(in), dimension(undf_wtheta) :: field2

  end subroutine testkern_w2v_code

end module testkern_w2v_mod
