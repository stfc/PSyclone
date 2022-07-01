!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
! -----------------------------------------------------------------------------
!
! BSD 3-Clause License
!
! Modifications copyright (c) 2022, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Modified by R. W. Ford, STFC Daresbury Lab

!> @brief Computes the tangent linear for hydrostatic balance.
!> @details The nonlinear term is
!!          \f  < \nabla . (cp \theta v) , \Pi > + < \nabla . v, \Phi > \f
!!          The linearized term is
!!          \f    < \nabla . (cp ls_\theta v) , \Pi >
!!             +  < \nabla . (cp \theta v) , ls_\Pi >     \f
module tl_hydrostatic_kernel_mod
  use argument_mod,      only : arg_type, func_type,       &
                                GH_FIELD, GH_REAL,         &
                                GH_SCALAR,                 &
                                GH_READ, GH_INC,           &
                                GH_BASIS, GH_DIFF_BASIS,   &
                                CELL_COLUMN,               &
                                GH_QUADRATURE_XYoZ,        &
                                ANY_W2
  use constants_mod,     only : r_def
  use fs_continuity_mod, only : W3, Wtheta
  use kernel_mod,        only : kernel_type
  implicit none
  private
  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> Psy layer.
  !>
  type, public, extends(kernel_type) :: tl_hydrostatic_kernel_type
    private
    type(arg_type) :: meta_args(8) = (/                      &
        arg_type(GH_FIELD,   GH_REAL, GH_INC,  ANY_W2),      &
        arg_type(GH_FIELD,   GH_REAL, GH_READ, W3),          &
        arg_type(GH_FIELD,   GH_REAL, GH_READ, Wtheta),      &
        arg_type(GH_FIELD*3, GH_REAL, GH_READ, Wtheta),      &
        arg_type(GH_FIELD,   GH_REAL, GH_READ, W3),          &
        arg_type(GH_FIELD,   GH_REAL, GH_READ, Wtheta),      &
        arg_type(GH_FIELD*3, GH_REAL, GH_READ, Wtheta),      &
        arg_type(GH_SCALAR,  GH_REAL, GH_READ)               &
        /)
    type(func_type) :: meta_funcs(3) = (/                &
        func_type(ANY_W2,      GH_BASIS, GH_DIFF_BASIS), &
        func_type(W3,          GH_BASIS),                &
        func_type(Wtheta,      GH_BASIS, GH_DIFF_BASIS)  &
        /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = GH_QUADRATURE_XYoZ
  contains
    procedure, nopass ::tl_hydrostatic_code
  end type
  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public tl_hydrostatic_code
contains
!> @brief Compute the tangent linear of the hydrostatic term.
!! @param[in] nlayers       Number of layers
!! @param[in,out] r_u       ACTIVE Change in Momentum equation right hand side
!! @param[in] exner         ACTIVE Change in Exner pressure field
!! @param[in] theta         ACTIVE Change in Potential temperature field
!! @param[in] moist_dyn_gas ACTIVE Change in Moist dynamics factor in gas law
!! @param[in] moist_dyn_tot ACTIVE Change in Moist dynamics total mass factor
!! @param[in] moist_dyn_fac ACTIVE Change in Moist dynamics water factor
!! @param[in] ls_exner         Lin state Exner pressure field
!! @param[in] ls_theta         Lin state Potential temperature field
!! @param[in] ls_moist_dyn_gas Lin state Moist dynamics factor in gas law
!! @param[in] ls_moist_dyn_tot Lin state Moist dynamics total mass factor
!! @param[in] ls_moist_dyn_fac Lin state Moist dynamics water factor
!! @param[in] cp            Specific heat of dry air at constant pressure
!! @param[in] ndf_w2        Number of degrees of freedom per cell for w2
!! @param[in] undf_w2       Number of unique degrees of freedom  for w2
!! @param[in] map_w2        Dofmap for the cell at the base of the column for w2
!! @param[in] w2_basis      Basis functions evaluated at quadrature points
!! @param[in] w2_diff_basis Differential of the basis functions evaluated
!!                          at quadrature points
!! @param[in] ndf_w3        Number of degrees of freedom per cell for w3
!! @param[in] undf_w3       Number of unique degrees of freedom  for w3
!! @param[in] map_w3        Dofmap for the cell at the base of the column for w3
!! @param[in] w3_basis      Basis functions evaluated at gaussian quadrature
!!                          points
!! @param[in] ndf_wt        Number of degrees of freedom per cell for wt
!! @param[in] undf_wt       Number of unique degrees of freedom  for wt
!! @param[in] map_wt        Dofmap for the cell at the base of the column for wt
!! @param[in] wt_basis      Basis functions evaluated at gaussian quadrature
!!                          points
!! @param[in] wt_diff_basis Differential of the basis functions evaluated at
!!                          quadrature points
!! @param[in] nqp_h         Number of quadrature points in the horizontal
!! @param[in] nqp_v         Number of quadrature points in the vertical
!! @param[in] wqp_h         Horizontal quadrature weights
!! @param[in] wqp_v         Vertical quadrature weights
subroutine tl_hydrostatic_code(nlayers,          &
                               r_u,              &
                               exner,            &
                               theta,            &
                               moist_dyn_gas,    &
                               moist_dyn_tot,    &
                               moist_dyn_fac,    &
                               ls_exner,         &
                               ls_theta,         &
                               ls_moist_dyn_gas, &
                               ls_moist_dyn_tot, &
                               ls_moist_dyn_fac, &
                               cp,               &
                               ndf_w2,           &
                               undf_w2,          &
                               map_w2,           &
                               w2_basis,         &
                               w2_diff_basis,    &
                               ndf_w3,           &
                               undf_w3,          &
                               map_w3,           &
                               w3_basis,         &
                               ndf_wt,           &
                               undf_wt,          &
                               map_wt,           &
                               wt_basis,         &
                               wt_diff_basis,    &
                               nqp_h,            &
                               nqp_v,            &
                               wqp_h,            &
                               wqp_v             &
                               )
  implicit none
  ! Arguments
  integer, intent(in) :: nlayers,nqp_h, nqp_v
  integer, intent(in) :: ndf_wt, ndf_w2, ndf_w3
  integer, intent(in) :: undf_wt, undf_w2, undf_w3
  integer, dimension(ndf_wt), intent(in) :: map_wt
  integer, dimension(ndf_w2), intent(in) :: map_w2
  integer, dimension(ndf_w3), intent(in) :: map_w3
  real(kind=r_def), dimension(1,ndf_w3,nqp_h,nqp_v), intent(in) :: w3_basis
  real(kind=r_def), dimension(3,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_basis
  real(kind=r_def), dimension(1,ndf_wt,nqp_h,nqp_v), intent(in) :: wt_basis
  real(kind=r_def), dimension(1,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_diff_basis
  real(kind=r_def), dimension(3,ndf_wt,nqp_h,nqp_v), intent(in) :: wt_diff_basis
  real(kind=r_def), dimension(undf_w2), intent(inout) :: r_u
  real(kind=r_def), dimension(undf_w3), intent(in)    :: exner
  real(kind=r_def), dimension(undf_wt), intent(in)    :: theta
  real(kind=r_def), dimension(undf_wt), intent(in)    :: moist_dyn_gas, &
                                                         moist_dyn_tot, &
                                                         moist_dyn_fac
  real(kind=r_def), dimension(undf_w3), intent(in)    :: ls_exner
  real(kind=r_def), dimension(undf_wt), intent(in)    :: ls_theta
  real(kind=r_def), dimension(undf_wt), intent(in)    :: ls_moist_dyn_gas, &
                                                         ls_moist_dyn_tot, &
                                                         ls_moist_dyn_fac
  real(kind=r_def),                     intent(in)    :: cp
  real(kind=r_def), dimension(nqp_h), intent(in)      ::  wqp_h
  real(kind=r_def), dimension(nqp_v), intent(in)      ::  wqp_v
  ! Internal variables
  integer               :: df, k
  integer               :: qp1, qp2
  real(kind=r_def), dimension(ndf_w3)          :: exner_e
  real(kind=r_def), dimension(ndf_wt)          :: theta_v_e
  real(kind=r_def), dimension(ndf_w3)          :: ls_exner_e
  real(kind=r_def), dimension(ndf_wt)          :: ls_theta_v_e
  real(kind=r_def) :: grad_theta_v_at_quad(3), ls_grad_theta_v_at_quad(3), v(3)
  real(kind=r_def) :: exner_at_quad, theta_v_at_quad,       &
                      ls_exner_at_quad, ls_theta_v_at_quad, &
                      grad_term, dv
  do k = 0, nlayers-1
    ! Linearisation state
    do df = 1, ndf_w3
      ls_exner_e(df) = ls_exner( map_w3(df) + k )
    end do
    do df = 1, ndf_wt
      ls_theta_v_e(df) = ls_theta( map_wt(df) + k ) * &
                         ls_moist_dyn_gas( map_wt(df) + k ) / &
                         ls_moist_dyn_tot( map_wt(df) + k )
    end do
    ! Perturbation
    do df = 1, ndf_w3
      exner_e(df) = exner( map_w3(df) + k )
    end do
    do df = 1, ndf_wt
      theta_v_e(df) = ls_theta_v_e(df) * &
         ( theta( map_wt(df) + k ) /  ls_theta( map_wt(df) + k )  +               &
           moist_dyn_gas( map_wt(df) + k ) / ls_moist_dyn_gas( map_wt(df) + k ) - &
           moist_dyn_tot( map_wt(df) + k ) / ls_moist_dyn_tot( map_wt(df) + k ) )
    end do
    ! Compute the RHS integrated over one cell
    do qp2 = 1, nqp_v
      do qp1 = 1, nqp_h
        ! Linearisation state
        ls_exner_at_quad = 0.0_r_def
        do df = 1, ndf_w3
          ls_exner_at_quad  = ls_exner_at_quad + ls_exner_e(df)*w3_basis(1,df,qp1,qp2)
        end do
        ls_theta_v_at_quad = 0.0_r_def
        ls_grad_theta_v_at_quad(:) = 0.0_r_def
        do df = 1, ndf_wt
          ls_theta_v_at_quad   = ls_theta_v_at_quad                                 &
                            + ls_theta_v_e(df)*wt_basis(1,df,qp1,qp2)
          ls_grad_theta_v_at_quad(:) = ls_grad_theta_v_at_quad(:)                   &
                                  + ls_theta_v_e(df)*wt_diff_basis(:,df,qp1,qp2)
        end do
        ! Perturbation
        exner_at_quad = 0.0_r_def
        do df = 1, ndf_w3
          exner_at_quad  = exner_at_quad + exner_e(df)*w3_basis(1,df,qp1,qp2)
        end do
        theta_v_at_quad = 0.0_r_def
        grad_theta_v_at_quad(:) = 0.0_r_def
        do df = 1, ndf_wt
          theta_v_at_quad   = theta_v_at_quad                                 &
                            + theta_v_e(df)*wt_basis(1,df,qp1,qp2)
          grad_theta_v_at_quad(:) = grad_theta_v_at_quad(:)                   &
                                   + theta_v_e(df)*wt_diff_basis(:,df,qp1,qp2)
        end do
        ! Calculation
        do df = 1, ndf_w2
          v  = w2_basis(:,df,qp1,qp2)
          dv = w2_diff_basis(1,df,qp1,qp2)
          ! Pressure gradient term
          grad_term = cp * ls_exner_at_quad * (                        &
                      theta_v_at_quad * dv                             &
                    + dot_product( grad_theta_v_at_quad(:),v) ) +      &
                      cp * exner_at_quad * (                           &
                      ls_theta_v_at_quad * dv                          &
                    + dot_product( ls_grad_theta_v_at_quad(:),v) )
          r_u( map_w2(df) + k ) = r_u( map_w2(df) + k ) &
                                + wqp_h(qp1)*wqp_v(qp2)*grad_term
        end do
      end do
    end do
  end do
end subroutine tl_hydrostatic_code
end module tl_hydrostatic_kernel_mod
