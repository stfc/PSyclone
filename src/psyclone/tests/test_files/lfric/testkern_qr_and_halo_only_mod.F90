! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Test kernel that operates only on cell columns and requires quadrature.
module testkern_qr_and_halo_only_mod

    use constants_mod
    use argument_mod
    use fs_continuity_mod
    use kernel_mod
  
    implicit none
  
    type, extends(kernel_type) :: testkern_qr_and_halo_only_type
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
       integer :: operates_on = halo_cell_column
       integer :: gh_shape = gh_quadrature_XYoZ
     contains
       procedure, nopass :: code => testkern_qr_and_halo_only_code
    end type testkern_qr_and_halo_only_type
  
  contains
  
    subroutine testkern_qr_and_halo_only_code(nlayers, halo_depth,         &
                                f1, f2, f3, ascalar, f4, iscalar,          &
                                ndf_w1, undf_w1, map_w1, basis_w1, ndf_w2, &
                                undf_w2, map_w2, diff_basis_w2, ndf_w3,    &
                                undf_w3, map_w3, basis_w3, diff_basis_w3,  &
                                nqp_h, nqp_v, wh, wv)
  
      implicit none
  
      integer(kind=i_def), intent(in) :: nlayers, iscalar, halo_depth
      integer(kind=i_def), intent(in) :: ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3
      integer(kind=i_def), intent(in) :: nqp_h, nqp_v
      integer(kind=i_def), intent(in), dimension(:) :: map_w1, map_w2, map_w3
      real(kind=r_def), intent(in) :: ascalar
      real(kind=r_def), dimension(:), intent(inout) :: f1
      real(kind=r_def), dimension(:), intent(in) :: f2, f3, f4
      real(kind=r_def), dimension(:), intent(in) :: wh, wv
      real(kind=r_def), dimension(:,:,:,:), intent(in) :: basis_w1, diff_basis_w2
      real(kind=r_def), dimension(:,:,:,:), intent(in) :: basis_w3, diff_basis_w3
  
    end subroutine testkern_qr_and_halo_only_code
  
  end module testkern_qr_and_halo_only_mod
  
