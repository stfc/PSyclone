! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_stencil_multi_int_field_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  ! Integer-valued fields on discontinuous (w2broken and w2v) and
  ! continuous (w1 and w0) function spaces with different stencil accesses
  type, extends(kernel_type) :: testkern_stencil_multi_int_field_type
     type(arg_type), dimension(4) :: meta_args =                                 &
          (/ arg_type(gh_field, gh_integer, gh_readwrite, w2broken),             &
             arg_type(gh_field, gh_integer, gh_read,      w1,  stencil(cross)),  &
             arg_type(gh_field, gh_integer, gh_read,      w0,  stencil(xory1d)), &
             arg_type(gh_field, gh_integer, gh_read,      w2v, stencil(x1d))     &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_stencil_multi_int_field_code
  end type testkern_stencil_multi_int_field_type

contains

  subroutine testkern_stencil_multi_int_field_code(nlayers, fld1,                  &
                                        fld2, fld2_st_size, fld2_st_dofmap,        &
                                        fld3, fld3_st_size,                        &
                                        fld3_direction, fld3_st_dofmap,            &
                                        fld4, fld4_st_size, fld4_st_dofmap,        &
                                        ndf_w2broken, undf_w2broken, map_w2broken, &
                                        ndf_w1, undf_w1, map_w1,                   &
                                        ndf_w0, undf_w0, map_w0,                   &
                                        ndf_w2v, undf_w2v, map_w2v)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2broken
    integer(kind=i_def), intent(in) :: ndf_w2v
    integer(kind=i_def), intent(in) :: undf_w2broken, undf_w1, undf_w0, undf_w2v
    integer(kind=i_def), intent(in) :: fld2_st_size, fld3_st_size, fld4_st_size
    integer(kind=i_def), intent(in) :: fld3_direction
    integer(kind=i_def), intent(in),    dimension(ndf_w1,fld2_st_size)  :: fld2_st_dofmap
    integer(kind=i_def), intent(in),    dimension(ndf_w0,fld3_st_size)  :: fld3_st_dofmap
    integer(kind=i_def), intent(in),    dimension(ndf_w2v,fld4_st_size) :: fld4_st_dofmap
    integer(kind=i_def), intent(in),    dimension(ndf_w0)       :: map_w0
    integer(kind=i_def), intent(in),    dimension(ndf_w1)       :: map_w1
    integer(kind=i_def), intent(in),    dimension(ndf_w2broken) :: map_w2broken
    integer(kind=i_def), intent(in),    dimension(ndf_w2v)      :: map_w2v
    integer(kind=i_def), intent(inout), dimension(undf_w2broken) :: fld1
    integer(kind=i_def), intent(in),    dimension(undf_w1)       :: fld2
    integer(kind=i_def), intent(in),    dimension(undf_w0)       :: fld3
    integer(kind=i_def), intent(in),    dimension(undf_w2v)      :: fld4

  end subroutine testkern_stencil_multi_int_field_code

end module testkern_stencil_multi_int_field_mod
