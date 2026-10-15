! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! A kernel which writes to a field on 'any_space'. Such a kernel must guarantee
! that any writes to a given shared entity are for the same value and that the
! first access to such an entity is a write.
module testkern_write_any_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  ! Description: function spaces with one continuous ('any_space_1')
  ! field writer.
  type, extends(kernel_type) :: testkern_write_any_type
     type(arg_type), dimension(2) :: meta_args = (/                        &
          arg_type(gh_field, gh_real, gh_write, any_space_1),              &
          arg_type(gh_field, gh_real, gh_read,  w2)                        &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_write_any_code
  end type testkern_write_any_type

contains

  subroutine testkern_write_any_code(nlayers,                             &
                                     field1, field2,                      &
                                     ndf_aspc1, undf_aspc1, map_aspc1,    &
                                     ndf_w2, undf_w2, map_w2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_aspc1, undf_w2
    integer(kind=i_def), intent(in), dimension(ndf_aspc1)  :: map_aspc1
    integer(kind=i_def), intent(in), dimension(ndf_w2)     :: map_w2
    real(kind=r_def), intent(out), dimension(undf_aspc1)   :: field1
    real(kind=r_def), intent(in), dimension(undf_w2)       :: field2

  end subroutine testkern_write_any_code

end module testkern_write_any_mod
