!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Computes the tangent linear for the boundary integral part of
!>        rhs of the momentum equation for the nonlinear equations.
!>
!> @details This consists of change in pressure_gradient_bd =
!>          - cp * ls_theta_e * v * normal_vector * average(pi)
!>          - cp * theta_e * v * normal_vector * average(ls_pi)
!>          where ls_theta_e = ls_theta * (ls_mg /ls_mt)
!>          theta_e =
!>             - ( ls_theta * (ls_mg /ls_mt) * (mt / ls_mt) )
!>             + ( ls_theta * (mg / ls_mt) )
!>             + ( theta * (ls_mg /ls_mt) )
module tl_pressure_gradient_bd_kernel_mod

  use argument_mod,             only : arg_type, func_type,         &
                                       mesh_data_type,              &
                                       reference_element_data_type, &
                                       GH_FIELD, GH_READ, GH_INC,   &
                                       GH_SCALAR,                   &
                                       GH_REAL, STENCIL, CROSS,     &
                                       GH_BASIS, GH_DIFF_BASIS,     &
                                       GH_QUADRATURE_face,          &
                                       CELL_COLUMN, adjacent_face,  &
                                       outward_normals_to_horizontal_faces
  use constants_mod,            only : r_def, i_def
  use cross_product_mod,        only : cross_product
  use fs_continuity_mod,        only : W2, W3, Wtheta
  use kernel_mod,               only : kernel_type

  implicit none

  private

  !-------------------------------------------------------------------------------
  ! Public types
  !-------------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the PSy layer
  type, public, extends(kernel_type) :: tl_pressure_gradient_bd_kernel_type
    private
    type(arg_type) :: meta_args(8) = (/                                     &
         arg_type(GH_FIELD,   GH_REAL, GH_INC,  W2),                        &
         arg_type(GH_FIELD,   GH_REAL, GH_READ, W3, STENCIL(CROSS)),        &
         arg_type(GH_FIELD,   GH_REAL, GH_READ, Wtheta),                    &
         arg_type(GH_FIELD*3, GH_REAL, GH_READ, Wtheta),                    &
         arg_type(GH_FIELD,   GH_REAL, GH_READ, W3),                        &
         arg_type(GH_FIELD,   GH_REAL, GH_READ, Wtheta),                    &
         arg_type(GH_FIELD*3, GH_REAL, GH_READ, Wtheta),                    &
         arg_type(GH_SCALAR,  GH_REAL, GH_READ)                             &
         /)
    type(func_type) :: meta_funcs(3) = (/                                   &
         func_type(W2,     GH_BASIS),                                       &
         func_type(W3,     GH_BASIS),                                       &
         func_type(Wtheta, GH_BASIS)                                        &
         /)
    type(mesh_data_type) :: meta_mesh(1) = (/                               &
         mesh_data_type( adjacent_face )                                    &
         /)
    type(reference_element_data_type) :: meta_reference_element(1) = (/     &
         reference_element_data_type( outward_normals_to_horizontal_faces ) &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = GH_QUADRATURE_face
  contains
    procedure, nopass :: tl_pressure_gradient_bd_code
  end type

  !-------------------------------------------------------------------------------
  ! Contained functions/subroutines
  !-------------------------------------------------------------------------------
  public tl_pressure_gradient_bd_code

contains
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Tangent linear for computing the boundary integral terms in
  !>        terms of the pressure gradient.
  !>
  !> @param[in] nlayers Number of layers
  !> @param[in,out] r_u_bd       ACTIVE Change in RHS of the momentum equation
  !> @param[in] exner            ACTIVE Change in Exner pressure
  !> @param[in] stencil_w3_size  Size of the W3 stencil (number of cells)
  !> @param[in] stencil_w3_map   W3 dofmaps for the stencil
  !> @param[in] theta            ACTIVE Change in Potential temperature
  !> @param[in] moist_dyn_gas    ACTIVE Change in Gas factor
  !> @param[in] moist_dyn_tot    ACTIVE Change in Total mass factor
  !> @param[in] moist_dyn_fac    ACTIVE Change in Water factor
  !> @param[in] ls_exner         Lin. state Exner pressure
  !> @param[in] ls_theta         Lin. state Potential temperature
  !> @param[in] ls_moist_dyn_gas Lin. state Gas factor (1 + m_v / epsilon)
  !> @param[in] ls_moist_dyn_tot Lin. state Total mass factor (1 + sum m_x)
  !> @param[in] ls_moist_dyn_fac Lin. state Water factor
  !> @param[in] cp               Specific heat of dry air at constant pressure
  !> @param[in] ndf_w2           Number of degrees of freedom per cell for W2
  !> @param[in] undf_w2          Number of unique degrees of freedom for W2
  !> @param[in] map_w2           Dofmap for the cell at the base of the
  !!                             column for W2
  !> @param[in] w2_basis_face    W2 basis functions evaluated at Gaussian
  !!                             quadrature points on horizontal faces
  !> @param[in] ndf_w3           Number of degrees of freedom per cell for W3
  !> @param[in] undf_w3          Number of unique degrees freedom for W3
  !> @param[in] map_w3           Dofmap for the cell at the base of the
  !!                             column for W3
  !> @param[in] w3_basis_face    W3 basis functions evaluated at Gaussian
  !!                             quadrature points on horizontal faces
  !> @param[in] ndf_wtheta       Number of degrees of freedom per cell for Wtheta
  !> @param[in] undf_wtheta      Number of unique degrees of freedom for Wtheta
  !> @param[in] map_wtheta       Dofmap for the cell at the base of the
  !!                             column for Wtheta
  !> @param[in] wtheta_basis_face Wtheta basis functions evaluated at Gaussian
  !!                              quadrature points on horizontal faces
  !> @param[in] nfaces_re_h      Number of reference element faces bisected by a
  !!                             horizontal plane
  !> @param[in] outward_normals_to_horizontal_faces Vector of normals to the
  !!                                                reference element horizontal
  !!                                                "outward faces"
  !> @param[in] opposite_face    Vector containing information on neighbouring
  !!                             face index for the current cell
  !> @param[in] nfaces_qr        Number of faces in the quadrature rule
  !> @param[in] nqp_f            Number of quadrature points on horizontal faces
  !> @param[in] wqp_f            Quadrature weights on horizontal faces
  !>
  subroutine tl_pressure_gradient_bd_code( nlayers,         &
                                        r_u_bd,             &
                                        exner,              &
                                        stencil_w3_size,    &
                                        stencil_w3_map,     &
                                        theta,              &
                                        moist_dyn_gas,      &
                                        moist_dyn_tot,      &
                                        moist_dyn_fac,      &
                                        ls_exner,           &
                                        ls_theta,           &
                                        ls_moist_dyn_gas,   &
                                        ls_moist_dyn_tot,   &
                                        ls_moist_dyn_fac,   &
                                        cp,                 &
                                        ndf_w2,             &
                                        undf_w2,            &
                                        map_w2,             &
                                        w2_basis_face,      &
                                        ndf_w3,             &
                                        undf_w3,            &
                                        map_w3,             &
                                        w3_basis_face,      &
                                        ndf_wtheta,         &
                                        undf_wtheta,        &
                                        map_wtheta,         &
                                        wtheta_basis_face,  &
                                        nfaces_re_h,        &
                                        outward_normals_to_horizontal_faces, &
                                        opposite_face,      &
                                        nfaces_qr,          &
                                        nqp_f,              &
                                        wqp_f )

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: nfaces_qr, nqp_f
    integer(kind=i_def), intent(in) :: ndf_w2, ndf_w3
    integer(kind=i_def), intent(in) :: undf_w2, undf_w3
    integer(kind=i_def), intent(in) :: ndf_wtheta, undf_wtheta
    integer(kind=i_def), intent(in) :: nfaces_re_h

    integer(kind=i_def), intent(in) :: stencil_w3_size
    integer(kind=i_def), dimension(ndf_w3,stencil_w3_size), intent(in) :: stencil_w3_map

    integer(kind=i_def), dimension(ndf_w2),     intent(in) :: map_w2
    integer(kind=i_def), dimension(ndf_w3),     intent(in) :: map_w3
    integer(kind=i_def), dimension(ndf_wtheta), intent(in) :: map_wtheta

    real(kind=r_def), dimension(3,ndf_w2,nqp_f,nfaces_qr),     intent(in) :: w2_basis_face
    real(kind=r_def), dimension(1,ndf_w3,nqp_f,nfaces_qr),     intent(in) :: w3_basis_face
    real(kind=r_def), dimension(1,ndf_wtheta,nqp_f,nfaces_qr), intent(in) :: wtheta_basis_face
    real(kind=r_def), dimension(nqp_f,nfaces_qr),              intent(in) :: wqp_f

    integer(kind=i_def), intent(in) :: opposite_face(:)

    real(kind=r_def),    intent(in) :: outward_normals_to_horizontal_faces(:,:)

    real(kind=r_def), dimension(undf_w2),     intent(inout) :: r_u_bd
    real(kind=r_def), dimension(undf_w3),     intent(in)    :: exner
    real(kind=r_def), dimension(undf_wtheta), intent(in)    :: theta
    real(kind=r_def), dimension(undf_wtheta), intent(in)    :: moist_dyn_gas, &
                                                               moist_dyn_tot, &
                                                               moist_dyn_fac
    real(kind=r_def), dimension(undf_w3),     intent(in)    :: ls_exner
    real(kind=r_def), dimension(undf_wtheta), intent(in)    :: ls_theta
    real(kind=r_def), dimension(undf_wtheta), intent(in)    :: ls_moist_dyn_gas, &
                                                               ls_moist_dyn_tot, &
                                                               ls_moist_dyn_fac
    real(kind=r_def),                         intent(in)    :: cp

    ! Internal variables
    integer(kind=i_def) :: df, k, face, face_next
    integer(kind=i_def) :: qp

    real(kind=r_def), dimension(ndf_w3)     :: exner_e, exner_next_e
    real(kind=r_def), dimension(ndf_wtheta) :: theta_v_e
    real(kind=r_def), dimension(ndf_w2)     :: pressure_gradient_bd_e
    real(kind=r_def), dimension(ndf_w3)     :: ls_exner_e, ls_exner_next_e
    real(kind=r_def), dimension(ndf_wtheta) :: ls_theta_v_e

    real(kind=r_def) :: v(3)
    real(kind=r_def) :: exner_av
    real(kind=r_def) :: theta_v_at_fquad, bdary_term
    real(kind=r_def) :: ls_exner_av
    real(kind=r_def) :: ls_theta_v_at_fquad

    do k = 0, nlayers-1

      do df = 1, ndf_w2
          pressure_gradient_bd_e(df) = 0.0_r_def
      end do

      do face = 1, nfaces_re_h

        ! Storing opposite face number on neighbouring cell
        face_next = opposite_face(face)

        ! Linearisation state
        do df = 1, ndf_w3
          ls_exner_e(df)      = ls_exner( stencil_w3_map(df, 1) + k )
          ls_exner_next_e(df) = ls_exner( stencil_w3_map(df, face+1) + k )
        end do
        do df = 1, ndf_wtheta
          ls_theta_v_e(df) = ls_theta( map_wtheta(df) + k )         &
                           * ls_moist_dyn_gas( map_wtheta(df) + k ) &
                           / ls_moist_dyn_tot( map_wtheta(df) + k )
        end do

        ! Perturbation
        do df = 1, ndf_w3
          exner_e(df)      = exner( stencil_w3_map(df, 1) + k )
          exner_next_e(df) = exner( stencil_w3_map(df, face+1) + k )
        end do
        do df = 1, ndf_wtheta
          theta_v_e(df) = ls_theta_v_e(df) * &
             ( theta( map_wtheta(df) + k ) /  ls_theta( map_wtheta(df) + k )  +               &
               moist_dyn_gas( map_wtheta(df) + k ) / ls_moist_dyn_gas( map_wtheta(df) + k ) - &
               moist_dyn_tot( map_wtheta(df) + k ) / ls_moist_dyn_tot( map_wtheta(df) + k ) )
        end do

        ! Compute the boundary RHS integrated over one horizontal face
        do qp = 1, nqp_f

          ! Linearisation state
          ls_exner_av = 0.0_r_def
          do df = 1, ndf_w3
            ls_exner_av = ls_exner_av + 0.5_r_def *                                         &
                                      (ls_exner_e(df)*w3_basis_face(1,df,qp,face)           &
                                     + ls_exner_next_e(df)*w3_basis_face(1,df,qp,face_next))
          end do
          ls_theta_v_at_fquad = 0.0_r_def
          do df = 1, ndf_wtheta
            ls_theta_v_at_fquad = ls_theta_v_at_fquad                              &
                                + ls_theta_v_e(df)*wtheta_basis_face(1,df,qp,face)
          end do

          ! Perturbation
          exner_av = 0.0_r_def
          do df = 1, ndf_w3
            exner_av = exner_av + 0.5_r_def*(exner_e(df)     *w3_basis_face(1,df,qp,face)       &
                                           + exner_next_e(df)*w3_basis_face(1,df,qp,face_next))
          end do
          theta_v_at_fquad = 0.0_r_def
          do df = 1, ndf_wtheta
            theta_v_at_fquad = theta_v_at_fquad + theta_v_e(df)*wtheta_basis_face(1,df,qp,face)
          end do

         ! Calculation
          do df = 1, ndf_w2
            v = w2_basis_face(:,df,qp,face)

            bdary_term = - cp * dot_product(v, outward_normals_to_horizontal_faces(:, face)) * &
                           ls_theta_v_at_fquad * exner_av                                      &
                         - cp * dot_product(v, outward_normals_to_horizontal_faces(:, face)) * &
                           theta_v_at_fquad * ls_exner_av

            pressure_gradient_bd_e(df) = pressure_gradient_bd_e(df) + wqp_f(qp,face) * bdary_term
          end do

        end do ! qp

      end do ! faces

      do df = 1, ndf_w2
        r_u_bd( map_w2(df) + k ) =  r_u_bd( map_w2(df) + k ) + pressure_gradient_bd_e(df)
      end do

    end do ! layers

  end subroutine tl_pressure_gradient_bd_code

end module tl_pressure_gradient_bd_kernel_mod
