! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module kernel_stencil_not_parallel
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod

  implicit none

  private

  public stencil_not_parallel, stencil_not_parallel_code

  type, extends(kernel_type) :: stencil_not_parallel
     type(go_arg), dimension(2) :: meta_args =    &
          ! We deliberately specify an incorrect stencil value
          ! for the first kernel argument in order to test the 
          ! parser: stencil accesses are not permitted on variables
          ! that are written to."

          (/ go_arg(GO_READWRITE, GO_CT, GO_STENCIL(010,010,010)),  & ! u
             go_arg(GO_WRITE,  GO_CT, GO_POINTWISE)   & ! v
           /)
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => stencil_not_parallel_code
  end type stencil_not_parallel

contains

  !===================================================

  !> Some dummy operation, that cannot be executed in parallel
  !! due to stencil read and write access
  subroutine stencil_not_parallel_code(i, j, u, v)
    implicit none
    integer,  intent(in) :: i, j
    real(go_wp), intent(inout), dimension(:,:) :: u
    real(go_wp), intent(in),  dimension(:,:) :: v
    real(go_wp) :: tmp

    v(i,j) = u(i, j-1) + u(i,j) + u(i, j+1)
    u(i,j) = u(i,j) / 3
    v(i,j) = v(i,j) + 1

  end subroutine stencil_not_parallel_code

end module kernel_stencil_not_parallel
