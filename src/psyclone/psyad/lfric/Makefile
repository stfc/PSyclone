TL_DIR = tangent_linear
TL_TWEAKED_DIR = tangent_linear_tweaked
ADJ_PARTIAL_DIR = adjoint_partial

all: ${ADJ_PARTIAL_DIR}/adj_dg_inc_matrix_vector_kernel_mod.F90 ${ADJ_PARTIAL_DIR}/adj_moist_dyn_mass_kernel_mod.F90 ${ADJ_PARTIAL_DIR}/adj_project_eos_pressure_kernel_mod.F90 ${ADJ_PARTIAL_DIR}/adj_hydrostatic_kernel_mod.F90 ${ADJ_PARTIAL_DIR}/adj_transpose_matrix_vector_kernel_mod.F90 ${ADJ_PARTIAL_DIR}/adj_kinetic_energy_gradient_kernel_mod.F90 ${ADJ_PARTIAL_DIR}/adj_vorticity_advection_kernel_mod.F90 ${ADJ_PARTIAL_DIR}/adj_moist_dyn_gas_kernel_mod.F90 ${ADJ_PARTIAL_DIR}/adj_pressure_gradient_bd_kernel_mod.F90 ${ADJ_PARTIAL_DIR}/adj_matrix_vector_kernel_mod.F90

# Kernels that psyad can process

${ADJ_PARTIAL_DIR}/adj_dg_inc_matrix_vector_kernel_mod.F90: ${TL_DIR}/dg_inc_matrix_vector_kernel_mod.F90
	psyad $< -oad $@ -a lhs x lhs_e x_e

${ADJ_PARTIAL_DIR}/adj_moist_dyn_mass_kernel_mod.F90: ${TL_DIR}/tl_moist_dyn_mass_kernel_mod.F90
	psyad $< -oad $@ -a moist_dyn_tot mr_v mr_cl mr_r mr_ci mr_s mr_g mr_v_at_dof mr_cl_at_dof mr_r_at_dof mr_ci_at_dof mr_s_at_dof mr_g_at_dof

${ADJ_PARTIAL_DIR}/adj_project_eos_pressure_kernel_mod.F90: ${TL_DIR}/tl_project_eos_pressure_kernel_mod.F90
	psyad $< -oad $@ -a exner rho exner_e r_exner exner_at_quad rho_at_quad rho_e # theta theta_vd_e theta_vd_at_quad moist_dyn_gas

${ADJ_PARTIAL_DIR}/adj_hydrostatic_kernel_mod.F90: ${TL_DIR}/tl_hydrostatic_kernel_mod.F90
	psyad $< -oad $@ -a r_u exner theta grad_term exner_at_quad theta_v_at_quad res_dot_product grad_theta_v_at_quad theta_v_e exner_e moist_dyn_tot moist_dyn_gas

${ADJ_PARTIAL_DIR}/adj_transpose_matrix_vector_kernel_mod.F90: ${TL_DIR}/transpose_matrix_vector_kernel_mod.F90
	psyad $< -oad $@ -a lhs lhs_e x_e x

${ADJ_PARTIAL_DIR}/adj_pressure_gradient_bd_kernel_mod.F90: ${TL_DIR}/tl_pressure_gradient_bd_kernel_mod.F90
	psyad $< -oad $@ -a r_u_bd exner pressure_gradient_bd_e bdary_term exner_av theta_v_at_fquad theta_v_e exner_e exner_next_e theta moist_dyn_tot moist_dyn_gas

${ADJ_PARTIAL_DIR}/adj_matrix_vector_kernel_mod.F90: ${TL_DIR}/matrix_vector_kernel_mod.F90
	psyad $< -oad $@ -a lhs x lhs_e x_e

# Kernels that psyad can process after the code has been tweaked

${ADJ_PARTIAL_DIR}/adj_kinetic_energy_gradient_kernel_mod.F90: ${TL_TWEAKED_DIR}/tl_kinetic_energy_gradient_kernel_mod_tweaked.F90
	psyad $< -oad $@ -a r_u ru_e ke_at_quad res_dot_product mul2 u_at_quad u_e u

${ADJ_PARTIAL_DIR}/adj_vorticity_advection_kernel_mod.F90: ${TL_TWEAKED_DIR}/tl_vorticity_advection_kernel_mod_tweaked.F90
	psyad $< -oad $@ -a r_u res_dot_product vorticity_term cross_product1 cross_product2 j_vorticity u_at_quad mul2 vorticity_at_quad wind vorticity

${ADJ_PARTIAL_DIR}/adj_moist_dyn_gas_kernel_mod.F90: ${TL_TWEAKED_DIR}/tl_moist_dyn_gas_kernel_mod_tweaked.F90
	psyad $< -oad $@ -a moist_dyn_gas mr_v mr_v_at_dof

clean:
	rm -f ${ADJ_PARTIAL_DIR}/ad*.F90

# Kernels that require no modification (as tl == ad)
# enforce_bc_kernel_mod.F90

# Not in previous list - needed?
# psyad transpose_matrix_kernel_mod.F90

# Does not exist
# tl_rhs_eos_kernel_mod.F90
