! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_chi_read_mod

  use argument_mod
  use kernel_mod
  use fs_continuity_mod
  use constants_mod

  type, extends(kernel_type) :: testkern_chi_read_type
     type(arg_type), dimension(2) :: meta_args =          &
          (/ arg_type(gh_field,   gh_real, gh_inc,  w0),  &
             arg_type(gh_field*3, gh_real, gh_read, wchi) &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_chi_read_code
  end type testkern_chi_read_type

contains

  subroutine testkern_chi_read_code(nlayers, field1, field2_v1, &
                                    field2_v2, field2_v3,       &
                                    ndf_w0, undf_w0, map_w0,    &
                                    ndf_wchi, undf_wchi, map_wchi)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_wchi
    integer(kind=i_def), intent(in) :: undf_w0, undf_wchi
    integer(kind=i_def), intent(in), dimension(ndf_w0)   :: map_w0
    integer(kind=i_def), intent(in), dimension(ndf_wchi) :: map_wchi
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field1
    real(kind=r_def), intent(in), dimension(undf_wchi)  :: field2_v1
    real(kind=r_def), intent(in), dimension(undf_wchi)  :: field2_v2
    real(kind=r_def), intent(in), dimension(undf_wchi)  :: field2_v3

  end subroutine testkern_chi_read_code

end module testkern_chi_read_mod
