! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> Test kernel which operates on the domain and has an argument with
!! a cross2d stencil access.
module testkern_domain_stencil_mod
  use argument_mod,      only: arg_type, GH_FIELD, GH_REAL, &
                               GH_READ, GH_READWRITE, DOMAIN, &
                               STENCIL, CROSS2D, XORY1D, X1D
  use fs_continuity_mod, only: W3
  use kernel_mod,        only: kernel_type
  use constants_mod,     only: i_def, r_def

  implicit none

  type, extends(kernel_type) :: testkern_domain_stencil_type
     type(arg_type), dimension(4) :: meta_args =                         &
          (/ arg_type(gh_field, gh_real, gh_readwrite, w3),              &
             arg_type(gh_field, gh_real, gh_read, w3, stencil(cross2d)), &
             arg_type(gh_field, gh_real, gh_read, w3, stencil(xory1d)),  &
             arg_type(gh_field, gh_real, gh_read, w3, stencil(x1d))      &
           /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_stencil_code
  end type testkern_domain_stencil_type

contains

  subroutine testkern_domain_stencil_code(          &
       nlayers, ncell_2d, a,                        &
       b, b_st_size, b_max_branch_len, b_st_dofmap, &
       c, c_st_size, c_direction, c_st_dofmap,      &
       d, d_st_size, d_st_dofmap,                   &
       ndf_w3, undf_w3, map_w3)
    implicit none
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_2d
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: b_st_size, c_st_sice, d_st_size
    integer(kind=i_def), intent(in) :: c_direction
    integer(kind=i_def), intent(in), dimension(ndf_w3, ncell_2d) :: map_w3
    real(kind=r_def), intent(inout) :: a(:)
    real(kind=r_def), intent(in) :: b(:)
    integer(kind=i_def), intent(in) :: b_st_size(4, ncell_2d)
    integer(kind=i_def), intent(in) :: b_max_branch_len
    integer(kind=i_def), intent(in) :: b_st_dofmap(ndf_w3,b_max_branch_len,4,ncell_2d)
    real(kind=r_def),    intent(in) :: c(:)
    integer(kind=i_def), intent(in) :: c_st_size(ncell_2d)
    integer(kind=i_def), intent(in) :: c_st_dofmap(ndf_w3,4,ncell_2d)
    real(kind=r_def),    intent(in) :: d(:)
    integer(kind=i_def), intent(in) :: d_st_size(ncell_2d)
    integer(kind=i_def), intent(in) :: d_st_dofmap(ndf_w3,4,ncell_2d)
  end subroutine testkern_domain_stencil_code

end module testkern_domain_stencil_mod
