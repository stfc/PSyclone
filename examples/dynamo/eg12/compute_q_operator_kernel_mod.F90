!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2019, Science and Technology Facilities Council
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
! Modified by I. Kavcic, Met Office
!
module compute_q_operator_kernel_mod

use argument_mod,      only: arg_type, func_type,   &
                             GH_OPERATOR, GH_FIELD, &
                             GH_READ, GH_WRITE,     &
                             ANY_SPACE_1,           &
                             GH_BASIS,              &
                             CELLS, GH_QUADRATURE_XYoZ
use constants_mod,     only: r_def, i_def
use fs_continuity_mod, only: W2
use kernel_mod,        only: kernel_type

implicit none

! Precomputed arrays
real(kind=r_def), allocatable, private :: delta_z(:)

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: compute_q_operator_type
  private
  type(arg_type) :: meta_args(1) = (/                                  &
       arg_type(GH_OPERATOR, GH_WRITE, W2, ANY_SPACE_1)                &
       /)
  type(func_type) :: meta_funcs(2) = (/                                &
       func_type(W2,          GH_BASIS),                               &
       func_type(ANY_SPACE_1, GH_BASIS)                                &
       /)
  integer :: iterates_over = CELLS
  integer :: gh_shape = GH_QUADRATURE_XYoZ
contains
  procedure, nopass :: compute_q_operator_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface compute_q_operator_type
   module procedure compute_q_operator_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public compute_q_operator_init
public compute_q_operator_code
public compute_q_operator_final

contains

type(compute_q_operator_type) &
                       function compute_q_operator_constructor() result(self)
  return
end function compute_q_operator_constructor

!> @brief Computes the q operator which is the projection of the vertical
!!        bouyancy term
!! @param[in] cell cell id
!! @param[in] nlayers number of layers.
!! @param[in] ncell_3d ncell*nlayers
!! @param[in] ndf_w2 number of degrees of freedom per cell.
!! @param[in] basis_w2 basis functions evaluated at quadrature points.
!! @param[in] ndf_wt number of degrees of freedom per cell.
!! @param[in] basis_wt basis functions evaluated at quadrature points.
!! @param[in] q local stencil of the q operator
!! @param[in] nqp_h number of horizontal quadrature points
!! @param[in] nqp_v number of vertical quadrature points
!! @param[in] wqp_h Horizontal quadrature weights
!! @param[in] wqp_v Vertical quadrature weights
subroutine compute_q_operator_code(cell, nlayers, ncell_3d,     &
                                   q,                           &
                                   ndf_w2, basis_w2,            &
                                   ndf_wt, basis_wt,            &
                                   nqp_h, nqp_v, wqp_h, wqp_v )

  !Arguments
  integer(kind=i_def),                    intent(in) :: cell, nqp_h, nqp_v
  integer(kind=i_def),                    intent(in) :: nlayers
  integer(kind=i_def),                    intent(in) :: ncell_3d
  integer(kind=i_def),                    intent(in) :: ndf_wt, ndf_w2

  real(kind=r_def), intent(in) :: basis_wt(1,ndf_wt,nqp_h,nqp_v)
  real(kind=r_def), intent(in) :: basis_w2(3,ndf_w2,nqp_h,nqp_v)

  real(kind=r_def), dimension(ndf_w2,ndf_wt,ncell_3d), intent(inout) :: q
  real(kind=r_def), dimension(nqp_h),                  intent(in)    :: wqp_h
  real(kind=r_def), dimension(nqp_v),                  intent(in)    :: wqp_v

  !Internal variables
  integer(kind=i_def)                          :: df2, dft, k, ik
  integer(kind=i_def)                          :: qp1, qp2
  real(kind=r_def)                             :: integrand
  real(kind=r_def), dimension(3)               :: z_hat(3)


  do k = 0, nlayers - 1
    z_hat = (/ 0.0_r_def, 0.0_r_def, delta_z(k+1) /)
    ik = k + 1 + (cell-1)*nlayers
    do dft = 1, ndf_wt
      do df2 = 1, ndf_w2
        q(df2,dft,ik) = 0.0_r_def
        do qp2 = 1, nqp_v
          do qp1 = 1, nqp_h
            integrand = wqp_h(qp1)*wqp_v(qp2)                               &
                      *dot_product(basis_w2(:,df2,qp1,qp2),z_hat)           &
                      *basis_wt(1,dft,qp1,qp2)
            q(df2,dft,ik) = q(df2,dft,ik) + integrand
          end do
        end do
      end do
    end do
  end do
end subroutine compute_q_operator_code

!=============================================================================!
!>@brief Initialise the q computation kernel, copies the dz array into a local
!!       copy
!!@param[in] dz layer thickness array
!!@param[in] nlayers the number of layers
subroutine compute_q_operator_init(dz, nlayers)

  implicit none

  integer(kind=i_def),                  intent(in) :: nlayers
  real(kind=r_def), dimension(nlayers), intent(in) :: dz

  allocate( delta_z(nlayers) )
  delta_z = dz
end subroutine compute_q_operator_init

!=============================================================================!
!>@brief Reclaims memory from private allocatable arrays
subroutine compute_q_operator_final()

  implicit none

  if (allocated(delta_z)) deallocate (delta_z)

end subroutine compute_q_operator_final

end module compute_q_operator_kernel_mod
