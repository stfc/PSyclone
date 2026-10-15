! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module kernel_utf_char_mod
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  private

  public kernel_utf_char, kernel_utf_char_code

  type, extends(kernel_type) :: kernel_utf_char
     type(go_arg), dimension(3) :: meta_args =      &
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE), &
             go_arg(GO_READ,  GO_CV, GO_POINTWISE), &
             go_arg(GO_READ,  GO_CT, GO_POINTWISE)  &
           /)
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     integer :: index_offset = GO_OFFSET_ANY

  contains
    procedure, nopass :: code => kernel_utf_char_code
 end type kernel_utf_char

contains

  !===================================================

  SUBROUTINE kernel_utf_char_code(i, j, u, v, h)
    IMPLICIT none
    integer,  intent(in) :: I, J
    REAL(go_wp), INTENT(inout), DIMENSION(:,:) :: u
    REAL(go_wp), INTENT(in),    DIMENSION(:,:) :: h, v

    write(*,*) 'max reachable coeff. (at the Equator) for e1=1°)'
    u(I,J) = h(I,J)+.25d0*(U(I+1,J)*U(I+1,J)+U(I,J)*U(I,J) + & 
                           V(I,J+1)*V(I,J+1)+V(I,J)*V(I,J))

  END SUBROUTINE kernel_utf_char_code

END MODULE kernel_utf_char_mod
