! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module init_field_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod, only: GO_OFFSET_SW
  implicit none

  type, extends(kernel_type) :: init_field
     type(go_arg), dimension(2) :: meta_args =             &
          (/ go_arg(GO_WRITE, GO_CT,       GO_POINTWISE),  & ! field
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE)   & ! value
           /)
     !> This kernel writes to all points of the simulation domain.
     integer :: ITERATES_OVER = GO_ALL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => init_field_code
  end type init_field

contains

  subroutine init_field_code(i, j, fld1, value)
    integer, intent(in) :: i, j
    real(go_wp), dimension(:,:), intent(inout) :: fld1
    real, intent(in) :: value
    fld1(i,j) = value
    
  end subroutine init_field_code

end module init_field_mod
