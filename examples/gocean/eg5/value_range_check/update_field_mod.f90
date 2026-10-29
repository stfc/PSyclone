! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> This module contains a simple 'update' kernel that
!! creates some NAN values in the output field by
!! calling some division by 0. If the compiler should
!! catch floating point exceptions, the code for checking
!! for NANs will not be executed.

module update_field_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod, only: GO_OFFSET_SW
  implicit none

  type, extends(kernel_type) :: update_field
     type(go_arg), dimension(2) :: meta_args =                 &
          (/ go_arg(GO_READWRITE, GO_CT,       GO_POINTWISE),  & ! field
             go_arg(GO_READ,      GO_CT,       GO_POINTWISE)   & ! field
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

  subroutine update_field_code(i, j, a_f, b_f)
    integer, intent(in) :: i, j
    real(go_wp), dimension(:,:), intent(inout) :: a_f
    real(go_wp), dimension(:,:), intent(in) :: b_f

    ! Trigger a NAN result on the diagonal:
    a_f(i,j) = b_f(i,j) / (i-j)

  end subroutine update_field_code

end module update_field_mod
