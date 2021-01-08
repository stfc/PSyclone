! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2021, Science and Technology Facilities Council
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
! Author R. W. Ford STFC Daresbury Lab
! Modified I. Kavcic Met Office
module testkern_qr

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_qr_type
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
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_qr_code
  end type testkern_qr_type

contains

  subroutine testkern_qr_code(nlayers, f1, f2, f3, ascalar, f4, iscalar, &
                              ndf_w1, undf_w1, map_w1, basis_w1, ndf_w2, &
                              undf_w2, map_w2, diff_basis_w2, ndf_w3,    &
                              undf_w3, map_w3, basis_w3, diff_basis_w3,  &
                              nqp_h, nqp_v, wh, wv)

    implicit none

    integer(kind=i_def) :: nlayers, iscalar
    integer(kind=i_def) :: ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3
    integer(kind=i_def) :: nqp_h, nqp_v
    integer(kind=i_def), dimension(:) :: map_w1, map_w2, map_w3
    real(kind=r_def) :: ascalar
    real(kind=r_def), dimension(:) :: f1, f2, f3, f4
    real(kind=r_def), dimension(:) :: wh, wv
    real(kind=r_def), dimension(:,:,:,:) :: basis_w1, diff_basis_w2
    real(kind=r_def), dimension(:,:,:,:) :: basis_w3, diff_basis_w3

  end subroutine testkern_qr_code

end module testkern_qr
