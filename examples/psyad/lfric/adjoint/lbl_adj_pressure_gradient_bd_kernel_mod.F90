module adj_pressure_gradient_bd_kernel_mod
  use argument_mod, only : adjacent_face, arg_type, cell_column, cross, func_type, &
       gh_basis, gh_diff_basis, gh_field, gh_inc, gh_quadrature_face, gh_read, &
       gh_readwrite, gh_real, gh_scalar, mesh_data_type, &
       outward_normals_to_horizontal_faces, reference_element_data_type, stencil
  use constants_mod, only : i_def, r_def
  use cross_product_mod, only : cross_product
  use fs_continuity_mod, only : w2, w3, wtheta
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_pressure_gradient_bd_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(8) = (/arg_type(GH_FIELD, GH_REAL, GH_READ, W2), arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W3, &
&STENCIL(CROSS)), arg_type(GH_FIELD, GH_REAL, GH_READWRITE, Wtheta), arg_type(GH_FIELD * 3, GH_REAL, GH_READWRITE, Wtheta), &
&arg_type(GH_FIELD, GH_REAL, GH_READ, W3), arg_type(GH_FIELD, GH_REAL, GH_READ, Wtheta), arg_type(GH_FIELD * 3, GH_REAL, GH_READ, &
&Wtheta), arg_type(GH_SCALAR, GH_REAL, GH_READ)/)
  TYPE(func_type) :: meta_funcs(3) = (/func_type(W2, GH_BASIS), func_type(W3, GH_BASIS), func_type(Wtheta, GH_BASIS)/)
  TYPE(mesh_data_type) :: meta_mesh(1) = (/mesh_data_type(adjacent_face)/)
  TYPE(reference_element_data_type) :: meta_reference_element(1) = &
&(/reference_element_data_type(outward_normals_to_horizontal_faces)/)
  INTEGER :: operates_on = CELL_COLUMN
  INTEGER :: gh_shape = GH_QUADRATURE_face
  CONTAINS
  PROCEDURE, NOPASS :: adj_pressure_gradient_bd_code
END TYPE
  private

  public :: adj_pressure_gradient_bd_code

  contains
  subroutine adj_pressure_gradient_bd_code(nlayers, r_u_bd, exner, stencil_w3_size, stencil_w3_map, theta, moist_dyn_gas, &
&moist_dyn_tot, moist_dyn_fac, ls_exner, ls_theta, ls_moist_dyn_gas, ls_moist_dyn_tot, ls_moist_dyn_fac, cp, ndf_w2, undf_w2, &
&map_w2, w2_basis_face, ndf_w3, undf_w3, map_w3, w3_basis_face, ndf_wtheta, undf_wtheta, map_wtheta, wtheta_basis_face, &
&nfaces_re_h, outward_normals_to_horizontal_faces, opposite_face, nfaces_qr, nqp_f, wqp_f)
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: nfaces_qr
    integer(kind=i_def), intent(in) :: nqp_f
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in) :: undf_wtheta
    integer(kind=i_def), intent(in) :: nfaces_re_h
    integer(kind=i_def), intent(in) :: stencil_w3_size
    integer(kind=i_def), dimension(ndf_w3,stencil_w3_size), intent(in) :: stencil_w3_map
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    integer(kind=i_def), dimension(ndf_wtheta), intent(in) :: map_wtheta
    real(kind=r_def), dimension(3,ndf_w2,nqp_f,nfaces_qr), intent(in) :: w2_basis_face
    real(kind=r_def), dimension(1,ndf_w3,nqp_f,nfaces_qr), intent(in) :: w3_basis_face
    real(kind=r_def), dimension(1,ndf_wtheta,nqp_f,nfaces_qr), intent(in) :: wtheta_basis_face
    real(kind=r_def), dimension(nqp_f,nfaces_qr), intent(in) :: wqp_f
    integer(kind=i_def), dimension(:), intent(in) :: opposite_face
    real(kind=r_def), dimension(:,:), intent(in) :: outward_normals_to_horizontal_faces
    real(kind=r_def), dimension(undf_w2), intent(in) :: r_u_bd
    real(kind=r_def), dimension(undf_w3), intent(inout) :: exner
    real(kind=r_def), dimension(undf_wtheta), intent(inout) :: theta
    real(kind=r_def), dimension(undf_wtheta), intent(inout) :: moist_dyn_gas
    real(kind=r_def), dimension(undf_wtheta), intent(inout) :: moist_dyn_tot
    real(kind=r_def), dimension(undf_wtheta), intent(in) :: moist_dyn_fac
    real(kind=r_def), dimension(undf_w3), intent(in) :: ls_exner
    real(kind=r_def), dimension(undf_wtheta), intent(in) :: ls_theta
    real(kind=r_def), dimension(undf_wtheta), intent(in) :: ls_moist_dyn_gas
    real(kind=r_def), dimension(undf_wtheta), intent(in) :: ls_moist_dyn_tot
    real(kind=r_def), dimension(undf_wtheta), intent(in) :: ls_moist_dyn_fac
    real(kind=r_def), intent(in) :: cp
    integer(kind=i_def) :: df
    integer(kind=i_def) :: k
    integer(kind=i_def) :: face
    integer(kind=i_def) :: face_next
    integer(kind=i_def) :: qp
    real(kind=r_def), dimension(ndf_w3) :: exner_e
    real(kind=r_def), dimension(ndf_w3) :: exner_next_e
    real(kind=r_def), dimension(ndf_wtheta) :: theta_v_e
    real(kind=r_def), dimension(ndf_w2) :: pressure_gradient_bd_e
    real(kind=r_def), dimension(ndf_w3) :: ls_exner_e
    real(kind=r_def), dimension(ndf_w3) :: ls_exner_next_e
    real(kind=r_def), dimension(ndf_wtheta) :: ls_theta_v_e
    real(kind=r_def), dimension(3) :: v
    real(kind=r_def) :: exner_av
    real(kind=r_def) :: theta_v_at_fquad
    real(kind=r_def) :: bdary_term
    real(kind=r_def) :: ls_exner_av
    real(kind=r_def) :: ls_theta_v_at_fquad
    integer :: i
    real(kind=r_def) :: res_dot_product
    integer :: i_1
    real(kind=r_def) :: res_dot_product_1

    pressure_gradient_bd_e = 0.0_r_def
    bdary_term = 0.0_r_def
    exner_av = 0.0_r_def
    theta_v_at_fquad = 0.0_r_def
    theta_v_e = 0.0_r_def
    exner_e = 0.0_r_def
    exner_next_e = 0.0_r_def
    do k = nlayers - 1, 0, -1
      do df = ndf_w2, 1, -1
        pressure_gradient_bd_e(df) = pressure_gradient_bd_e(df) + r_u_bd(map_w2(df) + k)
      enddo
      do face = nfaces_re_h, 1, -1
        face_next = opposite_face(face)
        do df = 1, ndf_w3, 1
          ls_exner_e(df) = ls_exner(stencil_w3_map(df,1) + k)
          ls_exner_next_e(df) = ls_exner(stencil_w3_map(df,face + 1) + k)
        enddo
        do df = 1, ndf_wtheta, 1
          ls_theta_v_e(df) = ls_moist_dyn_gas(k + map_wtheta(df)) * ls_theta(k + map_wtheta(df)) / ls_moist_dyn_tot(k + &
&map_wtheta(df))
        enddo
        do qp = nqp_f, 1, -1
          ls_exner_av = 0.0_r_def
          do df = 1, ndf_w3, 1
            ls_exner_av = ls_exner_av + 0.5 * ls_exner_e(df) * w3_basis_face(1,df,qp,face) + 0.5 * ls_exner_next_e(df) * &
&w3_basis_face(1,df,qp,face_next)
          enddo
          ls_theta_v_at_fquad = 0.0_r_def
          do df = 1, ndf_wtheta, 1
            ls_theta_v_at_fquad = ls_theta_v_at_fquad + ls_theta_v_e(df) * wtheta_basis_face(1,df,qp,face)
          enddo
          do df = ndf_w2, 1, -1
            v(:) = w2_basis_face(:,df,qp,face)
            res_dot_product = 0.0
            do i = 1, 3, 1
              res_dot_product = res_dot_product + outward_normals_to_horizontal_faces(i,face) * v(i)
            enddo
            res_dot_product_1 = 0.0
            do i_1 = 1, 3, 1
              res_dot_product_1 = res_dot_product_1 + outward_normals_to_horizontal_faces(i_1,face) * v(i_1)
            enddo
            bdary_term = bdary_term + pressure_gradient_bd_e(df) * wqp_f(qp,face)
            exner_av = exner_av + (-cp * bdary_term * ls_theta_v_at_fquad * res_dot_product)
            theta_v_at_fquad = theta_v_at_fquad - cp * ls_exner_av * res_dot_product_1 * bdary_term
            bdary_term = 0.0
          enddo
          do df = ndf_wtheta, 1, -1
            theta_v_e(df) = theta_v_e(df) + theta_v_at_fquad * wtheta_basis_face(1,df,qp,face)
          enddo
          theta_v_at_fquad = 0.0
          do df = ndf_w3, 1, -1
            exner_e(df) = exner_e(df) + 0.5 * exner_av * w3_basis_face(1,df,qp,face)
            exner_next_e(df) = exner_next_e(df) + 0.5 * exner_av * w3_basis_face(1,df,qp,face_next)
          enddo
          exner_av = 0.0
        enddo
        do df = ndf_wtheta, 1, -1
          theta(k + map_wtheta(df)) = theta(k + map_wtheta(df)) + ls_theta_v_e(df) * theta_v_e(df) / ls_theta(k + map_wtheta(df))
          moist_dyn_tot(k + map_wtheta(df)) = moist_dyn_tot(k + map_wtheta(df)) - ls_theta_v_e(df) * theta_v_e(df) / &
&ls_moist_dyn_tot(k + map_wtheta(df))
          moist_dyn_gas(k + map_wtheta(df)) = moist_dyn_gas(k + map_wtheta(df)) + ls_theta_v_e(df) * theta_v_e(df) / &
&ls_moist_dyn_gas(k + map_wtheta(df))
          theta_v_e(df) = 0.0
        enddo
        do df = ndf_w3, 1, -1
          exner(stencil_w3_map(df,face + 1) + k) = exner(stencil_w3_map(df,face + 1) + k) + exner_next_e(df)
          exner_next_e(df) = 0.0
          exner(stencil_w3_map(df,1) + k) = exner(stencil_w3_map(df,1) + k) + exner_e(df)
          exner_e(df) = 0.0
        enddo
      enddo
      do df = ndf_w2, 1, -1
        pressure_gradient_bd_e(df) = 0.0
      enddo
    enddo

  end subroutine adj_pressure_gradient_bd_code

end module adj_pressure_gradient_bd_kernel_mod
