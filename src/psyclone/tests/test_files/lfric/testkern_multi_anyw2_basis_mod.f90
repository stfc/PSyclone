! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_multi_anyw2_basis_mod

  use argument_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_multi_anyw2_basis_type
     type(arg_type), dimension(3) :: meta_args =           &
          (/ arg_type(gh_field, gh_real, gh_inc,  any_w2), &
             arg_type(gh_field, gh_real, gh_read, any_w2), &
             arg_type(gh_field, gh_real, gh_read, any_w2)  &
           /)
     type(func_type), dimension(1) :: meta_funcs =         &
          (/ func_type(any_w2, gh_basis, gh_diff_basis) /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_multi_anyw2_basis_code
  end type testkern_multi_anyw2_basis_type

contains

  subroutine testkern_multi_anyw2_basis_code(nlayers, f1, f2, f3, &
                             ndf_any_w2, undf_any_w2, map_any_w2, &
                             basis_any_w2, diff_basis_any_w2,     &
                             nqp_h, nqp_v, wh, wv)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_any_w2
    integer(kind=i_def), intent(in) :: undf_any_w2
    integer(kind=i_def), intent(in) :: nqp_h, nqp_v
    integer(kind=i_def), intent(in), dimension(ndf_any_w2) :: map_any_w2
    real(kind=r_def), intent(inout), dimension(undf_any_w2) :: f1
    real(kind=r_def), intent(in), dimension(undf_any_w2)    :: f2
    real(kind=r_def), intent(in), dimension(undf_any_w2)    :: f3
    real(kind=r_def), intent(in), dimension(3,ndf_any_w2,nqp_h,nqp_v) :: basis_any_w2
    real(kind=r_def), intent(in), dimension(1,ndf_any_w2,nqp_h,nqp_v) :: diff_basis_any_w2
    real(kind=r_def), intent(in), dimension(nqp_h) :: wh
    real(kind=r_def), intent(in), dimension(nqp_v) :: wv

  end subroutine testkern_multi_anyw2_basis_code

end module testkern_multi_anyw2_basis_mod
