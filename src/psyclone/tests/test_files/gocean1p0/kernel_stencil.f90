! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module kernel_stencil
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod

  implicit none

  private

  public compute_cu, compute_cu_code

  type, extends(kernel_type) :: compute_cu
     type(go_arg), dimension(4) :: meta_args =    &
          ! We deliberately specify an incorrect stencil value
          ! for the first kernel argument in order to test the 
          ! parser...
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),            & ! cu
             go_arg(GO_READ,  GO_CT, GO_STENCIL(000,011,000)), & ! p
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),            & ! u
             go_arg(GO_READ,  GO_GRID_AREA_T)                  &
           /)
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_cu_code
  end type compute_cu

contains

  !===================================================

  !> Compute the mass flux in the x direction at point (i,j)
  subroutine compute_cu_code(i, j, cu, p, u, area)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(out), dimension(:,:) :: cu
    real(go_wp), intent(in),  dimension(:,:) :: p, u
    ! While this kernel does not access 'area', the unit
    ! test 'gocean1p0_stencil_test.py' need this parameter
    ! in test 'test_stencil_information'.
    real(go_wp),  dimension(:,:), intent(in) :: area


    CU(I,J) = 0.5d0*(P(i+1,J)+P(I,J))*U(I,J)

  end subroutine compute_cu_code

end module kernel_stencil
