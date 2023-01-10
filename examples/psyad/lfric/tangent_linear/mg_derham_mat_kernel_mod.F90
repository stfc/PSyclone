!-----------------------------------------------------------------------------
! (C) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Computes the subset of mass matrices of the derham complex for Multigrid.
!>
!> @details Compute the mass matrices of the derham cochain,
!> these are the mass matrices for the W2, W3 and Wtheta function spaces
!> along with the divergence operator
!> Since they all depend upon the mesh Jacobian they are computed as one
!> to reduce the cost
!>
module mg_derham_mat_kernel_mod

  use argument_mod,            only: arg_type, func_type,        &
                                     GH_OPERATOR, GH_FIELD,      &
                                     GH_REAL, GH_READ, GH_WRITE, &
                                     ANY_SPACE_1, ANY_SPACE_9,   &
                                     ANY_DISCONTINUOUS_SPACE_3,  &
                                     GH_BASIS, GH_DIFF_BASIS,    &
                                     CELL_COLUMN, GH_QUADRATURE_XYoZ
  use constants_mod,           only: r_def, i_def
  use coordinate_jacobian_mod, only: pointwise_coordinate_jacobian, &
                                     pointwise_coordinate_jacobian_inverse
  use fs_continuity_mod,       only: W2, W3, wtheta
  use kernel_mod,              only: kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  type, public, extends(kernel_type) :: mg_derham_mat_kernel_type
    private
    type(arg_type) :: meta_args(6) = (/                                      &
         arg_type(GH_OPERATOR, GH_REAL, GH_WRITE, W2, W2),                   &
         arg_type(GH_OPERATOR, GH_REAL, GH_WRITE, W3, W3),                   &
         arg_type(GH_OPERATOR, GH_REAL, GH_WRITE, Wtheta, Wtheta),           &
         arg_type(GH_OPERATOR, GH_REAL, GH_WRITE, W3, W2),                   &
         arg_type(GH_FIELD*3,  GH_REAL, GH_READ,  ANY_SPACE_9),              &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  ANY_DISCONTINUOUS_SPACE_3) &
         /)
    type(func_type) :: meta_funcs(4) = (/                                    &
         func_type(W2,          GH_BASIS, GH_DIFF_BASIS),                    &
         func_type(W3,          GH_BASIS),                                   &
         func_type(Wtheta,      GH_BASIS),                                   &
         func_type(ANY_SPACE_9, GH_BASIS,  GH_DIFF_BASIS)                    &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = GH_QUADRATURE_XYoZ
  contains
    procedure, nopass :: mg_derham_mat_code
  end type

  public :: mg_derham_mat_code

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
contains

!> @brief This subroutine computes the operator matrices for the modified derham complex
!! @param[in] cell Cell number
!! @param[in] nlayers Number of layers
!! @param[in] ncell_3d2 ncell*nlayers
!! @param[in,out] mm2 Local assembly mass matrix for W2 space
!! @param[in] ncell_3d3 ncell*nlayers
!! @param[in,out] mm3 Local assembly mass matrix for W3 space
!! @param[in] ncell_3dt ncell*nlayers
!! @param[in,out] mmt Local assembly mass matrix for Wtheta space
!! @param[in] ncell_3d6 ncell*nlayers
!! @param[in,out] div Local assembly divergence matrix
!! @param[in] chi1 Physical coordinates in the 1st dir
!! @param[in] chi2 Physical coordinates in the 2nd dir
!! @param[in] chi3 Physical coordinates in the 3rd dir
!! @param[in] panel_id Field giving the ID for mesh panels
!! @param[in] ndf_w2 Number of degrees of freedom per cell for W2 space
!! @param[in] basis_w2 Basis functions evaluated at quadrature points for W2 space
!! @param[in] diff_basis_w2 Differential of basis functions evaluated at
!!                          quadrature points for W2 space
!! @param[in] ndf_w3 Number of degrees of freedom per cell for W3 space
!! @param[in] basis_w3 Basis functions evaluated at quadrature points for W3 space
!! @param[in] ndf_wt Number of degrees of freedom per cell for Wtheta space
!! @param[in] basis_wt Basis functions evaluated at quadrature points for Wtheta space
!! @param[in] ndf_chi Number of degrees of freedom per cell for chi field
!! @param[in] undf_chi Number of unique degrees of freedom for chi field
!! @param[in] map_chi Dofmap for the cell at the base of the column, for the
!!                    space on which the chi field lives
!! @param[in] basis_chi Basis functions for Wchi evaluated at quadrature points
!! @param[in] diff_basis_chi Vector differential basis functions evaluated at
!!                           quadrature points.
!! @param[in] ndf_pid  Number of degrees of freedom per cell for panel_id
!! @param[in] undf_pid Number of unique degrees of freedom for panel_id
!! @param[in] map_pid  Dofmap for the cell at the base of the column for panel_id
!! @param[in] nqp_h Number of horizontal quadrature points
!! @param[in] nqp_v Number of vertical quadrature points
!! @param[in] wqp_h Horizontal quadrature weights
!! @param[in] wqp_v Vertical quadrature weights
subroutine mg_derham_mat_code(cell, nlayers,                      &
                              ncell_3d2, mm2,                     &
                              ncell_3d3, mm3,                     &
                              ncell_3dt, mmt,                     &
                              ncell_3d6, div,                     &
                              chi1, chi2, chi3,                   &
                              panel_id,                           &
                              ndf_w2, basis_w2, diff_basis_w2,    &
                              ndf_w3, basis_w3,                   &
                              ndf_wt, basis_wt,                   &
                              ndf_chi, undf_chi,                  &
                              map_chi, basis_chi, diff_basis_chi, &
                              ndf_pid, undf_pid, map_pid,         &
                              nqp_h, nqp_v, wqp_h, wqp_v)

  implicit none

  ! Arguments
  integer(kind=i_def),   intent(in)     :: cell, nqp_h, nqp_v
  integer(kind=i_def),   intent(in)     :: nlayers
  integer(kind=i_def),   intent(in)     :: ndf_w2, ndf_w3, ndf_wt, ndf_pid
  integer(kind=i_def),   intent(in)     :: ncell_3d2, ncell_3d3, ncell_3dt, ncell_3d6
  integer(kind=i_def),   intent(in)     :: ndf_chi
  integer(kind=i_def),   intent(in)     :: undf_chi
  integer(kind=i_def),   intent(in)     :: undf_pid
  integer(kind=i_def), dimension(ndf_chi), intent(in) :: map_chi
  integer(kind=i_def), dimension(ndf_pid), intent(in) :: map_pid
  real(kind=r_def), intent(inout) :: mm2(ndf_w2,ndf_w2,ncell_3d2)
  real(kind=r_def), intent(inout) :: mm3(ndf_w3,ndf_w3,ncell_3d3)
  real(kind=r_def), intent(inout) :: mmt(ndf_wt,ndf_wt,ncell_3dt)
  real(kind=r_def), intent(inout) :: div(ndf_w3,ndf_w2,ncell_3d6)
  real(kind=r_def), intent(in)  :: basis_chi(1,ndf_chi,nqp_h,nqp_v)
  real(kind=r_def), intent(in)  :: diff_basis_chi(3,ndf_chi,nqp_h,nqp_v)
  real(kind=r_def), intent(in)  :: basis_w2(3,ndf_w2,nqp_h,nqp_v)
  real(kind=r_def), intent(in)  :: basis_w3(1,ndf_w3,nqp_h,nqp_v)
  real(kind=r_def), intent(in)  :: basis_wt(1,ndf_wt,nqp_h,nqp_v)
  real(kind=r_def), intent(in)  :: diff_basis_w2(1,ndf_w2,nqp_h,nqp_v)
  real(kind=r_def), intent(in)  :: chi1(undf_chi)
  real(kind=r_def), intent(in)  :: chi2(undf_chi)
  real(kind=r_def), intent(in)  :: chi3(undf_chi)
  real(kind=r_def), intent(in)  :: panel_id(undf_pid)
  real(kind=r_def), intent(in)  :: wqp_h(nqp_h)
  real(kind=r_def), intent(in)  :: wqp_v(nqp_v)

  ! Internal variables
  integer(kind=i_def)                          :: df, df2, k, ik
  integer(kind=i_def)                          :: qp1, qp2

  real(kind=r_def), dimension(ndf_chi)         :: chi1_e, chi2_e, chi3_e
  real(kind=r_def)                             :: integrand
  real(kind=r_def)                             :: dj
  real(kind=r_def), dimension(3,3)             :: jac
  real(kind=r_def), dimension(3)               :: jac_v
  real(kind=r_def)                             :: wt

  integer(kind=i_def) :: ipanel

  ipanel = int(panel_id(map_pid(1)), i_def)

  do k = 0, nlayers-1

     ! Indirect the chi coord field here
     do df = 1, ndf_chi
        chi1_e(df) = chi1(map_chi(df) + k)
        chi2_e(df) = chi2(map_chi(df) + k)
        chi3_e(df) = chi3(map_chi(df) + k)
     end do

     ik = 1 + k + (cell-1)*nlayers
     mm2(:,:,ik) = 0.0_r_def
     mm3(:,:,ik) = 0.0_r_def
     div(:,:,ik) = 0.0_r_def
     mmt(:,:,ik) = 0.0_r_def

     do qp2 = 1, nqp_v
        do qp1 = 1, nqp_h
           ! Precompute some frequently used terms
           call pointwise_coordinate_jacobian(ndf_chi, chi1_e, chi2_e, chi3_e, &
                                              ipanel, basis_chi(:,:,qp1,qp2),  &
                                              diff_basis_chi(:,:,qp1,qp2),     &
                                              jac, dj)
           wt = wqp_h(qp1)*wqp_v(qp2)

           ! W2 mass matrix
           do df2 = 1, ndf_w2
              jac_v = matmul(jac,basis_w2(:,df2,qp1,qp2))
              do df = df2, ndf_w2 ! mass matrix is symmetric
                 integrand = wt*                                  &
                      dot_product(                                &
                      matmul(jac,basis_w2(:,df,qp1,qp2)),jac_v    &
                      )/dj
                 mm2(df,df2,ik) = mm2(df,df2,ik) + integrand
              end do
           end do
           ! W3 mass matrix
           do df2 = 1, ndf_w3
              do df = df2, ndf_w3 ! mass matrix is symmetric
                 integrand = wt                                    &
                      *basis_w3(1,df,qp1,qp2)                &
                      *basis_w3(1,df2,qp1,qp2)*dj
                 mm3(df,df2,ik) = mm3(df,df2,ik) + integrand
              end do
           end do
           ! Wtheta mass matrix
           do df2 = 1, ndf_wt
              do df = df2, ndf_wt ! mass matrix is symmetric
                 integrand = wt                                    &
                      *basis_wt(1,df,qp1,qp2)                &
                      *basis_wt(1,df2,qp1,qp2)*dj
                 mmt(df,df2,ik) = mmt(df,df2,ik) + integrand
              end do
           end do
           ! Div matrix
           do df2 = 1, ndf_w2
              do df = 1, ndf_w3
                 integrand = wt                                    &
                      *basis_w3(1,df,qp1,qp2)                &
                      *diff_basis_w2(1,df2,qp1,qp2)!*dj
                 div(df,df2,ik) = div(df,df2,ik) + integrand
              end do
           end do
        end do
     end do

     do df2 = 1,ndf_w2
        do df = df2, 1, -1
           mm2(df,df2,ik) = mm2(df2,df,ik)
        end do
     end do
     do df2 = 1,ndf_w3
        do df = df2, 1, -1
           mm3(df,df2,ik) = mm3(df2,df,ik)
        end do
     end do
     do df2 = 1,ndf_wt
        do df = df2, 1, -1
           mmt(df,df2,ik) = mmt(df2,df,ik)
        end do
     end do
  end do ! end of k loop

end subroutine mg_derham_mat_code

end module mg_derham_mat_kernel_mod
