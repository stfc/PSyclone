! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module update_field_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod, only: GO_OFFSET_SW
  implicit none

  type, extends(kernel_type) :: update_field
     type(go_arg), dimension(8) :: meta_args =             &
          (/ go_arg(GO_READWRITE, GO_CT,       GO_POINTWISE),  & ! field
             go_arg(GO_READ,      GO_CT,       GO_POINTWISE),  & ! field
             go_arg(GO_READ,      GO_CT,       GO_POINTWISE),  & ! field
             go_arg(GO_READ,      GO_CT,       GO_POINTWISE),  & ! field
             go_arg(GO_READWRITE, GO_R_SCALAR, GO_POINTWISE),  & ! scalar
             go_arg(GO_WRITE,     GO_R_SCALAR, GO_POINTWISE),  & ! scalar
             go_arg(GO_READ,      GO_R_SCALAR, GO_POINTWISE),  & ! scalar
             go_Arg(GO_READ,      GO_GRID_DX_CONST)                  &
           /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
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
    procedure, nopass :: code => update_field_code
  end type update_field

contains

  subroutine update_field_code(i, j, a_f, b_f, c_f, d_f, x, y, z, grid_dx)
    integer, intent(in) :: i, j
    real(go_wp), dimension(:,:), intent(inout) :: a_f
    real(go_wp), dimension(:,:), intent(in) :: b_f, c_f, d_f
    real(go_wp), intent(inout) :: x
    real(go_wp), intent(out)   :: y
    real(go_wp), intent(in)    :: z, grid_dx

    x = x + z * grid_dx
    y = 2*z
    a_f(i,j) = a_f(i,j) + b_f(i,j) + c_f(i,j)*d_f(i,j)
    
  end subroutine update_field_code

end module update_field_mod
