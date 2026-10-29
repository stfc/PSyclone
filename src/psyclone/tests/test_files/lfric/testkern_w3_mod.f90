! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_w3_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  ! Description: discontinuous field (read)writer (w3)
  type, extends(kernel_type) :: testkern_w3_type
     type(arg_type), dimension(5) :: meta_args = (/       &
          arg_type(gh_scalar, gh_real, gh_read),          &
          arg_type(gh_field,  gh_real, gh_read,      w0), &
          arg_type(gh_field,  gh_real, gh_read,      w1), &
          arg_type(gh_field,  gh_real, gh_read,      w2), &
          arg_type(gh_field,  gh_real, gh_readwrite, w3)  &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_w3_code
  end type testkern_w3_type

contains

  subroutine testkern_w3_code(nlayers, ascalar,        &
                              fld1, fld2, fld3, fld4,  &
                              ndf_w0, undf_w0, map_w0, &
                              ndf_w1, undf_w1, map_w1, &
                              ndf_w2, undf_w2, map_w2, &
                              ndf_w3, undf_w3, map_w3)

    implicit none

    integer(kind=i_def), intent(in)  :: nlayers
    integer(kind=i_def), intent(in)  :: ndf_w0, undf_w0, &
                                        ndf_w1, undf_w1, &
                                        ndf_w2, undf_w2, &
                                        ndf_w3, undf_w3
    integer(kind=i_def), dimension(ndf_w1), intent(in) :: map_w0
    integer(kind=i_def), dimension(ndf_w1), intent(in) :: map_w1
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    real(kind=r_def), intent(in) :: ascalar
    real(kind=r_def), dimension(undf_w1), intent(in)    :: fld1
    real(kind=r_def), dimension(undf_w2), intent(in)    :: fld2
    real(kind=r_def), dimension(undf_w2), intent(in)    :: fld3
    real(kind=r_def), dimension(undf_w3), intent(inout) :: fld4

  end subroutine testkern_w3_code

end module testkern_w3_mod
