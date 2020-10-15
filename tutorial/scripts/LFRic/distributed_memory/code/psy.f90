  MODULE helmholtz_solver_alg_mod_psy
    USE constants_mod, ONLY: r_def, i_def
    USE field_mod, ONLY: field_type, field_proxy_type
    USE operator_mod, ONLY: operator_type, operator_proxy_type
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0(grad_p, p, div_star, hb_inv, hp, mt_lumped_inv, compound_div, p3theta, ptheta2, m3_exner_star, tau_t, timestep_term)
      USE apply_variable_hx_kernel_mod, ONLY: apply_variable_hx_code
      USE enforce_bc_kernel_mod, ONLY: enforce_bc_code
      USE scaled_matrix_vector_kernel_mod, ONLY: scaled_matrix_vector_code
      REAL(KIND=r_def), intent(in) :: tau_t, timestep_term
      TYPE(field_type), intent(in) :: grad_p, p, div_star, hb_inv, hp, mt_lumped_inv
      TYPE(operator_type), intent(in) :: compound_div, p3theta, ptheta2, m3_exner_star
      INTEGER(KIND=i_def) cell
      INTEGER df
      INTEGER(KIND=i_def) nlayers
      TYPE(operator_proxy_type) compound_div_proxy, p3theta_proxy, ptheta2_proxy, m3_exner_star_proxy
      TYPE(field_proxy_type) grad_p_proxy, p_proxy, div_star_proxy, hb_inv_proxy, hp_proxy, mt_lumped_inv_proxy
      INTEGER(KIND=i_def), pointer :: boundary_dofs_grad_p(:,:) => null()
      INTEGER(KIND=i_def), pointer :: map_aspc1_grad_p(:,:) => null(), map_aspc1_mt_lumped_inv(:,:) => null(), map_aspc2_p(:,:) => null(), map_w2(:,:) => null(), map_w3(:,:) => null()
      INTEGER(KIND=i_def) ndf_aspc1_grad_p, undf_aspc1_grad_p, ndf_aspc2_p, undf_aspc2_p, ndf_w3, undf_w3, ndf_w2, undf_w2, ndf_aspc1_mt_lumped_inv, undf_aspc1_mt_lumped_inv
      !
      ! Initialise field and/or operator proxies
      !
      grad_p_proxy = grad_p%get_proxy()
      p_proxy = p%get_proxy()
      div_star_proxy = div_star%get_proxy()
      hb_inv_proxy = hb_inv%get_proxy()
      hp_proxy = hp%get_proxy()
      mt_lumped_inv_proxy = mt_lumped_inv%get_proxy()
      compound_div_proxy = compound_div%get_proxy()
      p3theta_proxy = p3theta%get_proxy()
      ptheta2_proxy = ptheta2%get_proxy()
      m3_exner_star_proxy = m3_exner_star%get_proxy()
      !
      ! Initialise number of layers
      !
      nlayers = grad_p_proxy%vspace%get_nlayers()
      !
      ! Look-up dofmaps for each function space
      !
      map_aspc1_grad_p => grad_p_proxy%vspace%get_whole_dofmap()
      map_aspc2_p => p_proxy%vspace%get_whole_dofmap()
      map_w3 => div_star_proxy%vspace%get_whole_dofmap()
      map_w2 => grad_p_proxy%vspace%get_whole_dofmap()
      map_aspc1_mt_lumped_inv => mt_lumped_inv_proxy%vspace%get_whole_dofmap()
      boundary_dofs_grad_p => grad_p_proxy%vspace%get_boundary_dofs()
      !
      ! Initialise number of DoFs for aspc1_grad_p
      !
      ndf_aspc1_grad_p = grad_p_proxy%vspace%get_ndf()
      undf_aspc1_grad_p = grad_p_proxy%vspace%get_undf()
      !
      ! Initialise number of DoFs for aspc2_p
      !
      ndf_aspc2_p = p_proxy%vspace%get_ndf()
      undf_aspc2_p = p_proxy%vspace%get_undf()
      !
      ! Initialise number of DoFs for w3
      !
      ndf_w3 = div_star_proxy%vspace%get_ndf()
      undf_w3 = div_star_proxy%vspace%get_undf()
      !
      ! Initialise number of DoFs for w2
      !
      ndf_w2 = grad_p_proxy%vspace%get_ndf()
      undf_w2 = grad_p_proxy%vspace%get_undf()
      !
      ! Initialise number of DoFs for aspc1_mt_lumped_inv
      !
      ndf_aspc1_mt_lumped_inv = mt_lumped_inv_proxy%vspace%get_ndf()
      undf_aspc1_mt_lumped_inv = mt_lumped_inv_proxy%vspace%get_undf()
      !
      ! Call our kernels
      !
      DO df=1,undf_aspc1_grad_p
        grad_p_proxy%data(df) = 0.0_r_def
      END DO
      DO cell=1,grad_p_proxy%vspace%get_ncell()
        !
        CALL scaled_matrix_vector_code(nlayers, grad_p_proxy%data, p_proxy%data, div_star_proxy%data, hb_inv_proxy%data, ndf_aspc1_grad_p, undf_aspc1_grad_p, map_aspc1_grad_p(:,cell), ndf_aspc2_p, undf_aspc2_p, map_aspc2_p(:,cell), ndf_w3, undf_w3, map_w3(:,cell))
      END DO
      DO cell=1,grad_p_proxy%vspace%get_ncell()
        !
        CALL enforce_bc_code(nlayers, grad_p_proxy%data, ndf_aspc1_grad_p, undf_aspc1_grad_p, map_aspc1_grad_p(:,cell), boundary_dofs_grad_p)
      END DO
      DO cell=1,hp_proxy%vspace%get_ncell()
        !
        CALL apply_variable_hx_code(cell, nlayers, hp_proxy%data, grad_p_proxy%data, mt_lumped_inv_proxy%data, p_proxy%data, compound_div_proxy%ncell_3d, compound_div_proxy%local_stencil, p3theta_proxy%ncell_3d, p3theta_proxy%local_stencil, ptheta2_proxy%ncell_3d, ptheta2_proxy%local_stencil, m3_exner_star_proxy%ncell_3d, m3_exner_star_proxy%local_stencil, tau_t, timestep_term, ndf_w3, undf_w3, map_w3(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_aspc1_mt_lumped_inv, undf_aspc1_mt_lumped_inv, map_aspc1_mt_lumped_inv(:,cell))
      END DO
      !
    END SUBROUTINE invoke_0
  END MODULE helmholtz_solver_alg_mod_psy