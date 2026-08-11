! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module inc_field_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod, only: GO_OFFSET_SW
  implicit none

  type, extends(kernel_type) :: inc_field
     type(go_arg), dimension(5) :: meta_args =             &
          (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),        & ! field
             ! We have to pass in the extend of the field array as the NVIDIA
             ! compiler does not support assumed-size arguments in accelerator
             ! regions. Ultimately PSyclone will do this for us.
             go_arg(GO_READ,  GO_I_SCALAR, GO_POINTWISE),  & ! nx
             go_arg(GO_READ,  GO_I_SCALAR, GO_POINTWISE),  & ! ny
             go_arg(GO_READ,  GO_I_SCALAR, GO_POINTWISE),   & ! istp
             ! Not used in the kernel but added here to test grid arguments
             ! generation using the PSyIR backend
             go_arg(GO_READ,  GO_GRID_AREA_T)         &
           /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => inc_field_code
  end type inc_field

contains

  subroutine inc_field_code(ji, jj, fld1, nx, ny, istp, area_t)
    integer, intent(in) :: ji, jj, nx, ny
    real(go_wp), dimension(nx,ny), intent(inout) :: fld1
    real(go_wp), dimension(nx,ny), intent(inout) :: area_t
    integer, intent(in) :: istp

    fld1(ji,jj) = fld1(ji,jj) + real(istp, go_wp)
    
  end subroutine inc_field_code

end module inc_field_mod
