! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2025, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Authors: A. R. Porter and R. W. Ford, STFC Daresbury Lab
! Modified: I. Kavcic, Met Office

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
