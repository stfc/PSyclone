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
! Modifications copyright (c) 2017-2018, Science and Technology Facilities Council
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
!
!> @brief Compute the projection operator from the velocity space to
!!        the potential temperature space weighted by the potential temperature gradient
!> @details Compute the projection operator \f[<\gamma,\nabla(\theta*)v>\f]
!!          where v is in W2 and gamma is in the potential temperature space
module weighted_proj_theta2_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_OPERATOR, GH_FIELD, GH_READ, GH_WRITE,&
                                    ANY_SPACE_9, W2,                         &
                                    GH_BASIS, GH_DIFF_BASIS,                 &
                                    CELLS
use constants_mod,           only : r_def, i_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: weighted_proj_theta2_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/                                  &
       arg_type(GH_OPERATOR, GH_WRITE, ANY_SPACE_9, W2),               &
       arg_type(GH_FIELD,    GH_READ,  ANY_SPACE_9)                    &
       /)
  type(func_type) :: meta_funcs(2) = (/                                &
       func_type(ANY_SPACE_9, GH_BASIS, GH_DIFF_BASIS),                &
       func_type(W2, GH_BASIS)                                         &
       /)
  integer :: iterates_over = CELLS
  integer :: gh_shape = GH_QUADRATURE_XYoZ

contains
  procedure, nopass ::weighted_proj_theta2_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! Overload the default structure constructor for function space
interface weighted_proj_theta2_kernel_type
   module procedure weighted_proj_theta2_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public weighted_proj_theta2_code
contains

type(weighted_proj_theta2_kernel_type) function weighted_proj_theta2_kernel_constructor() result(self)
  return
end function weighted_proj_theta2_kernel_constructor

!> @brief Compute the weighted projection operator that maps from W2 to Wtheta
!! @param[in] cell Current cell index
!! @param[in] nlayers Number of layers
!! @param[in] ncell_3d Total number of cells in the 3d mesh
!! @param[inout] projection Locally assembled projection operator
!! @param[in] theta Potential temperature array
!! @param[in] ndf_wtheta Number of degrees of freedom per cell for wtheta
!! @param[in] undf_wtheta Number of unique degrees of freedom  for wtheta
!! @param[in] map_wtheta Dofmap for the cell at the base of the column for wtheta
!! @param[in] wtheta_basis Basis functions evaluated at gaussian quadrature points
!! @param[in] wtheta_diff_basis Differential basis functions evaluated at gaussian quadrature points
!! @param[in] ndf_w2 Number of degrees of freedom per cell for w2
!! @param[in] w2_basis Basis functions evaluated at gaussian quadrature points 
!! @param[in] nqp_h Number of horizontal quadrature points
!! @param[in] nqp_v Number of vertical quadrature points
!! @param[in] wqp_h Weights of the horizontal quadrature points
!! @param[in] wqp_v Weights of the vertical quadrature points
subroutine weighted_proj_theta2_code(cell, nlayers, ncell_3d,              &
                                     projection,                           &
                                     theta,                                &
                                     ndf_wtheta, undf_wtheta, map_wtheta,  &
                                     wtheta_basis, wtheta_diff_basis,      &
                                     ndf_w2, w2_basis,                     &
                                     nqp_h, nqp_v, wqp_h, wqp_v )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: cell, nlayers, ncell_3d, nqp_h, nqp_v
  integer(kind=i_def), intent(in) :: ndf_wtheta, ndf_w2, undf_wtheta

  integer(kind=i_def), dimension(ndf_wtheta), intent(in) :: map_wtheta

  real(kind=r_def), dimension(1,ndf_wtheta,nqp_h,nqp_v), intent(in) :: wtheta_basis
  real(kind=r_def), dimension(3,ndf_wtheta,nqp_h,nqp_v), intent(in) :: wtheta_diff_basis
  real(kind=r_def), dimension(3,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_basis 

  real(kind=r_def), dimension(ndf_wtheta,ndf_w2,ncell_3d), intent(inout) :: projection
  real(kind=r_def), dimension(undf_wtheta),                intent(in)    :: theta

  real(kind=r_def), dimension(nqp_h), intent(in) ::  wqp_h
  real(kind=r_def), dimension(nqp_v), intent(in) ::  wqp_v

  ! Internal variables
  integer(kind=i_def) :: df, k, ik, dft, df2
  integer(kind=i_def) :: qp1, qp2
  
  real(kind=r_def), dimension(ndf_wtheta) :: theta_e
  real(kind=r_def) :: grad_theta_at_quad(3)
  real(kind=r_def) :: integrand 
  
  do k = 0, nlayers-1
    ik = k + 1 + (cell-1)*nlayers
    do df = 1, ndf_wtheta
      theta_e(df)  = theta( map_wtheta(df) + k )
    end do

    do df2 = 1,ndf_w2
      do dft = 1,ndf_wtheta
        projection(dft,df2,ik) = 0.0_r_def
        do qp2 = 1, nqp_v
          do qp1 = 1, nqp_h
            grad_theta_at_quad(:) = 0.0_r_def
            do df = 1, ndf_wtheta
              grad_theta_at_quad(:) = grad_theta_at_quad(:) &
                                    + theta_e(df)*wtheta_diff_basis(:,df,qp1,qp2)
            end do
            integrand = wqp_h(qp1)*wqp_v(qp2)*wtheta_basis(1,dft,qp1,qp2) &
                      *dot_product(grad_theta_at_quad,w2_basis(:,df2,qp1,qp2))
            projection(dft,df2,ik) = projection(dft,df2,ik) + integrand
          end do
        end do
      end do
    end do
  end do
  
end subroutine weighted_proj_theta2_code

end module weighted_proj_theta2_kernel_mod
