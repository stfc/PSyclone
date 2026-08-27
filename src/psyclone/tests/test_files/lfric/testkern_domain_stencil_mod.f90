! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> Test kernel which operates on the domain and has an argument with
!! a cross2d stencil access.
module testkern_domain_stencil_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_domain_stencil_type
     type(arg_type), dimension(3) :: meta_args =                    &
          (/ arg_type(gh_field, gh_real, gh_readwrite, w3),         &
             arg_type(gh_field, gh_real, gh_read, w3, stencil(cross2d)), &
             arg_type(gh_field, gh_real, gh_read, w3)                &
           /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_stencil_code
  end type testkern_domain_stencil_type

contains

  subroutine testkern_domain_stencil_code(nlayers, ncell_2d, &
       a, b, b_st_size, b_max, b_st_dofmap, c)
    implicit none
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_2d
    real(kind=r_def), intent(inout) :: a(:)
    real(kind=r_def), intent(in) :: b(:,:)
    integer(kind=i_def), intent(in) :: b_st_size(:,:)
    integer(kind=i_def), intent(in) :: b_max
    integer(kind=i_def), intent(in) :: b_st_dofmap(:,:,:,:)
    real(kind=r_def), intent(in) :: c(:)
  end subroutine testkern_domain_stencil_code

end module testkern_domain_stencil_mod
