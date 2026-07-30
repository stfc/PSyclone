! -----------------------------------------------------------------------------
! Original under:
! (C) Crown copyright 2018 Met Office. All rights reserved.
! For further details please refer to Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! Modifications under:
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> @brief Example of a tangent-linear kernel (based on the strong curl kernel)
!>
module tl_testkern_mod

  use argument_mod,      only : arg_type, func_type,     &
                                GH_FIELD, GH_REAL,       &
                                GH_READ, GH_INC,         &
                                GH_DIFF_BASIS, GH_BASIS, &
                                CELL_COLUMN, GH_EVALUATOR
  use constants_mod,     only : r_def, i_def
  use fs_continuity_mod, only : W1, W2
  use kernel_mod,        only : kernel_type

  implicit none

  private

  type, public, extends(kernel_type) :: tl_testkern_type
    private
    type(arg_type) :: meta_args(2) = (/            &
         arg_type(GH_FIELD, GH_REAL, GH_INC,  W2), &
         arg_type(GH_FIELD, GH_REAL, GH_READ, W1)  &
         /)
    type(func_type) :: meta_funcs(2) = (/          &
         func_type(W2, GH_BASIS),                  &
         func_type(W1, GH_DIFF_BASIS)              &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = GH_EVALUATOR
  contains
    procedure, nopass :: tl_testkern_code
  end type

  public :: tl_testkern_code

contains

!> @brief Kernel to compute the strong curl: xi = curl(u)
!! @param[in] nlayers Number of layers
!! @param[in,out] xi Field to contain curl of u
!! @param[in] u Wind field
!! @param[in] ndf2 Number of degrees of freedom per cell for W2
!! @param[in] undf2 Number of unique degrees of freedom for W2
!! @param[in] map2 Dofmap for the cell at the base of the column for W2
!! @param[in] basis_w2 W2 basis functions evaluated at nodal points of W2
!! @param[in] ndf1 Number of degrees of freedom per cell for W1
!! @param[in] undf1 Number of unique degrees of freedom for W1
!! @param[in] map1 Dofmap for the cell at the base of the column for the field to be advected
!! @param[in] diff_basis_w1 Differential W1 basis functions evaluated at nodal points of W2
subroutine tl_testkern_code(nlayers,                         &
                            xi, u,                           &
                            ndf2, undf2, map2, basis_w2,     &
                            ndf1, undf1, map1, diff_basis_w1 &
                            )

  implicit none

  ! Arguments
  integer(kind=i_def),                      intent(in)    :: nlayers
  integer(kind=i_def),                      intent(in)    :: ndf1, undf1, &
                                                             ndf2, undf2
  integer(kind=i_def), dimension(ndf1),     intent(in)    :: map1
  integer(kind=i_def), dimension(ndf2),     intent(in)    :: map2
  real(kind=r_def), dimension(3,ndf2,ndf2), intent(in)    :: basis_w2
  real(kind=r_def), dimension(3,ndf1,ndf2), intent(in)    :: diff_basis_w1
  real(kind=r_def), dimension(undf2),       intent(inout) :: xi
  real(kind=r_def), dimension(undf1),       intent(in)    :: u

  ! Internal variables
  integer(kind=i_def)            :: df1, df2, k
  real(kind=r_def), dimension(3) :: curl_u

  do k = 0, nlayers-1
    do df2 = 1,ndf2
      curl_u(:) = 0.0_r_def
      do df1 = 1,ndf1
        curl_u(:) = curl_u(:) + u(map1(df1)+k)*diff_basis_w1(:,df1,df2)
      end do
      xi( map2(df2)+k ) = dot_product(basis_w2(:,df2,df2),curl_u)
    end do
  end do

end subroutine tl_testkern_code

end module tl_testkern_mod
