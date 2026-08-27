! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_domain_mod

  use argument_mod,      only: arg_type, GH_FIELD, GH_REAL, &
                               GH_READ, GH_READWRITE, DOMAIN
  use fs_continuity_mod, only: W3
  use kernel_mod,        only: kernel_type
  use constants_mod,     only: i_def, r_def

  implicit none

  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), dimension(2) :: meta_args =            &
          (/ arg_type(gh_scalar, gh_real, gh_read),         &
             arg_type(gh_field,  gh_real, gh_readwrite, w3) &
           /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type

contains

  subroutine testkern_domain_code(nlayers, ncell_2d, ascalar,     &
                                  fld1, ndf_w3, undf_w3, map_w3)
    implicit none

    integer(kind=i_def), intent(in) :: ncell_2d
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in), dimension(ndf_w3, ncell_2d) :: map_w3
    real(kind=r_def), intent(in) :: ascalar
    real(kind=r_def), intent(inout), dimension(undf_w3) :: fld1

  end subroutine testkern_domain_code

end module testkern_domain_mod
