!-------------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-------------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2020, Science and Technology Facilities Council
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
! Modified by R. W. Ford and A. R. Porter, STFC
!             I. Kavcic, Met Office
!
!-------------------------------------------------------------------------------

!> @brief Kernel which computes LHS of Galerkin projection and solves equation in W3 space

module w3_solver_kernel_mod

use kernel_mod,              only : kernel_type
use constants_mod,           only : r_def, i_def
use fs_continuity_mod,       only : W3, Wchi
use argument_mod,            only : arg_type, func_type,     &
                                    GH_FIELD, GH_REAL,       &
                                    GH_READ, GH_WRITE,       &
                                    GH_BASIS, GH_DIFF_BASIS, &
                                    GH_QUADRATURE_XYoZ, CELLS

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the PSy layer
type, public, extends(kernel_type) :: w3_solver_kernel_type
  private
  type(arg_type) :: meta_args(4) = (/        &
       arg_type(GH_FIELD,   GH_WRITE, W3),   &
       arg_type(GH_FIELD,   GH_READ,  W3),   &
       arg_type(GH_FIELD*3, GH_READ,  Wchi), &
       arg_type(GH_REAL,    GH_READ)         &
       /)
  type(func_type) :: meta_funcs(2) = (/      &
       func_type(W3,   GH_BASIS),            &
       func_type(Wchi, GH_DIFF_BASIS)        &
       /)
  integer :: gh_shape = GH_QUADRATURE_XYoZ
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: solver_w3_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public solver_w3_code

contains

!> @brief Invert and apply the W3 mass matrix
!> @param[in] nlayers Number of layers
!> @param[in,out] x Output vector
!> @param[in] rhs Input vector
!> @param[in] chi_1 X component of the chi coordinate field
!> @param[in] chi_2 Y component of the chi coordinate field
!> @param[in] chi_3 Z component of the chi coordinate field
!> @param[in] ascalar Example of a real, scalar argument
!> @param[in] ndf_w3 Number of degrees of freedom per cell for W3
!> @param[in] undf_w3 Total number of degrees of freedom for W3
!> @param[in] map_w3 Dofmap for the cell at the base of the column for W3
!> @param[in] w3_basis Basis functions evaluated at gaussian quadrature points
!> @param[in] ndf_chi Number of degrees of freedom per cell for Wchi
!> @param[in] undf_chi Total number of degrees of freedom for Wchi
!> @param[in] map_chi Dofmap for the cell at the base of the column for Wchi
!> @param[in] chi_diff_basis Basis functions evaluated at gaussian quadrature points
!> @param[in] nqp_h Number of horizontal quadrature points
!> @param[in] nqp_v Number of vertical quadrature points
!> @param[in] wqp_h Weights of the horizontal quadrature points
!> @param[in] wqp_v Weights of the vertical quadrature points
subroutine solver_w3_code(nlayers,                                        &
                          x, rhs,                                         &
                          chi_1, chi_2, chi_3, ascalar,                   &
                          ndf_w3, undf_w3, map_w3, w3_basis,              &
                          ndf_wchi, undf_wchi, map_wchi, wchi_diff_basis, &
                          nqp_h, nqp_v, wqp_h, wqp_v                      &
                         )

  use matrix_invert_mod,       only : matrix_invert
  use coordinate_jacobian_mod, only : coordinate_jacobian

  ! Needs to compute the integral of rho_df * P
  ! P_analytic over a single column

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers, nqp_h, nqp_v
  integer(kind=i_def), intent(in) :: ndf_w3, undf_w3, ndf_wchi, undf_wchi
  integer(kind=i_def), dimension(ndf_w3),   intent(in) :: map_w3
  integer(kind=i_def), dimension(ndf_wchi), intent(in) :: map_wchi

  real(kind=r_def), intent(in) :: ascalar
  real(kind=r_def), dimension(1,ndf_w3,nqp_h,nqp_v),   intent(in) :: w3_basis
  real(kind=r_def), dimension(3,ndf_wchi,nqp_h,nqp_v), intent(in) :: wchi_diff_basis
  real(kind=r_def), dimension(undf_w3),   intent(inout) :: x
  real(kind=r_def), dimension(undf_w3),   intent(in)    :: rhs
  real(kind=r_def), dimension(undf_wchi), intent(in)    :: chi_1, chi_2, chi_3
  real(kind=r_def), dimension(nqp_h),     intent(in)    ::  wqp_h
  real(kind=r_def), dimension(nqp_v),     intent(in)    ::  wqp_v

  ! Internal variables
  integer(kind=i_def) :: df1, df2, k
  integer(kind=i_def) :: qp1, qp2

  real(kind=r_def) :: x_e(ndf_w3), rhs_e(ndf_w3)
  real(kind=r_def) :: integrand
  real(kind=r_def), dimension(ndf_w3,ndf_w3)   :: mass_matrix_w3, inv_mass_matrix_w3
  real(kind=r_def), dimension(nqp_h,nqp_v)     :: dj
  real(kind=r_def), dimension(3,3,nqp_h,nqp_v) :: jac
  real(kind=r_def), dimension(ndf_wchi)        :: chi_1_e, chi_2_e, chi_3_e

  ! Compute the LHS integrated over one cell and solve
  do k = 0, nlayers-1
    do df1 = 1, ndf_wchi
      chi_1_e(df1) = chi_1( map_wchi(df1) + k)
      chi_2_e(df1) = chi_2( map_wchi(df1) + k)
      chi_3_e(df1) = chi_3( map_wchi(df1) + k)
    end do
    call coordinate_jacobian(ndf_wchi, nqp_h, nqp_v, chi_1_e, chi_2_e, chi_3_e, wchi_diff_basis, jac, dj)
    do df1 = 1, ndf_w3
       do df2 = 1, ndf_w3
          mass_matrix_w3(df1,df2) = 0.0_r_def
          do qp2 = 1, nqp_v
             do qp1 = 1, nqp_h
                integrand =  w3_basis(1,df1,qp1,qp2) * &
                             w3_basis(1,df2,qp1,qp2) * dj(qp1,qp2)
                 mass_matrix_w3(df1,df2) = mass_matrix_w3(df1,df2) &
                                         + wqp_h(qp1)*wqp_v(qp2)*integrand
             end do
          end do
       end do
       rhs_e(df1) = rhs(map_w3(df1)+k)
    end do
    call matrix_invert(mass_matrix_w3,inv_mass_matrix_w3,ndf_w3)
    x_e = matmul(inv_mass_matrix_w3,rhs_e)
    do df1 = 1,ndf_w3
      x(map_w3(df1)+k) = x_e(df1)
    end do
  end do

end subroutine solver_w3_code

end module w3_solver_kernel_mod
