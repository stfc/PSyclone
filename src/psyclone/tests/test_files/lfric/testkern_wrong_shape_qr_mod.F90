! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_wrong_shape_qr_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_wrong_shape_qr_type
     type(arg_type), dimension(6) :: meta_args =           &
          (/ arg_type(gh_field,  gh_real,    gh_inc,  w1), &
             arg_type(gh_field,  gh_real,    gh_read, w2), &
             arg_type(gh_field,  gh_real,    gh_read, w2), &
             arg_type(gh_scalar, gh_real,    gh_read),     &
             arg_type(gh_field,  gh_real,    gh_read, w3), &
             arg_type(gh_scalar, gh_integer, gh_read)      &
           /)
     type(func_type), dimension(3) :: meta_funcs =         &
          (/ func_type(w1, gh_basis),                      &
             func_type(w2, gh_diff_basis),                 &
             func_type(w3, gh_basis, gh_diff_basis)        &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_wrong
   contains
     procedure, nopass :: code => testkern_wrong_shape_qr_code
  end type testkern_wrong_shape_qr_type

contains

  subroutine testkern_wrong_shape_qr_code(nlayers, f1, f2, f3,     &
                                          ascalar, f4, iscalar,    &
                                          ndf_w1, undf_w1, map_w1, &
                                          basis_w1,                &
                                          ndf_w2, undf_w2, map_w2, &
                                          diff_basis_w2,           &
                                          ndf_w3, undf_w3, map_w3, &
                                          basis_w3, diff_basis_w3, &
                                          nqp_h, nqp_v, wh, wv)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w1, undf_w2, undf_w3
    integer(kind=i_def), intent(in) :: nqp_h, nqp_v
    integer(kind=i_def), intent(in) :: iscalar
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def), intent(in) :: ascalar
    real(kind=r_def), intent(inout), dimension(undf_w1) :: f1
    real(kind=r_def), intent(in), dimension(undf_w2)    :: f2
    real(kind=r_def), intent(in), dimension(undf_w2)    :: f3
    real(kind=r_def), intent(in), dimension(undf_w3)    :: f4
    real(kind=r_def), intent(in), dimension(3,ndf_w1,nqp_h,nqp_v) :: basis_w1
    real(kind=r_def), intent(in), dimension(1,ndf_w2,nqp_h,nqp_v) :: diff_basis_w2
    real(kind=r_def), intent(in), dimension(1,ndf_w3,nqp_h,nqp_v) :: basis_w3
    real(kind=r_def), intent(in), dimension(3,ndf_w3,nqp_h,nqp_v) :: diff_basis_w3
    real(kind=r_def), intent(in), dimension(nqp_h) :: wh
    real(kind=r_def), intent(in), dimension(nqp_v) :: wv

  end subroutine testkern_wrong_shape_qr_code

end module testkern_wrong_shape_qr_mod
