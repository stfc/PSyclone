ORIG_DIR = orig
AD_DIR = adjoint

all: ${AD_DIR}/ad_dg_inc_matrix_vector_kernel_mod.F90 ${AD_DIR}/ad_moist_dyn_mass_kernel_mod.F90 ${AD_DIR}/ad_project_eos_pressure_kernel_mod.F90 ${AD_DIR}/ad_hydrostatic_kernel_mod.F90 ${AD_DIR}/ad_transpose_matrix_vector_kernel_mod.F90

# Kernels that psyad can process

${AD_DIR}/ad_dg_inc_matrix_vector_kernel_mod.F90: ${ORIG_DIR}/dg_inc_matrix_vector_kernel_mod.F90
	psyad ${ORIG_DIR}/dg_inc_matrix_vector_kernel_mod.F90 -oad ${AD_DIR}/ad_dg_inc_matrix_vector_kernel_mod.F90 -a lhs x lhs_e x_e

${AD_DIR}/ad_moist_dyn_mass_kernel_mod.F90: ${ORIG_DIR}/tl_moist_dyn_mass_kernel_mod.F90
	psyad ${ORIG_DIR}/tl_moist_dyn_mass_kernel_mod.F90 -oad ${AD_DIR}/ad_moist_dyn_mass_kernel_mod.F90 -a moist_dyn_tot mr_v mr_cl mr_r mr_ci mr_s mr_g mr_v_at_dof mr_cl_at_dof mr_r_at_dof mr_ci_at_dof mr_s_at_dof mr_g_at_dof

${AD_DIR}/ad_project_eos_pressure_kernel_mod.F90: ${ORIG_DIR}/tl_project_eos_pressure_kernel_mod.F90
	psyad ${ORIG_DIR}/tl_project_eos_pressure_kernel_mod.F90 -oad ${AD_DIR}/ad_project_eos_pressure_kernel_mod.F90 -a exner rho exner_e r_exner exner_at_quad rho_at_quad rho_e

${AD_DIR}/ad_hydrostatic_kernel_mod.F90: ${ORIG_DIR}/tl_hydrostatic_kernel_mod.F90
	psyad ${ORIG_DIR}/tl_hydrostatic_kernel_mod.F90 -oad ${AD_DIR}/ad_hydrostatic_kernel_mod.F90 -a r_u exner theta grad_term exner_at_quad theta_v_at_quad res_dot_product grad_theta_v_at_quad theta_v_e exner_e moist_dyn_tot moist_dyn_gas

${AD_DIR}/ad_transpose_matrix_vector_kernel_mod.F90: ${ORIG_DIR}/transpose_matrix_vector_kernel_mod.F90
	psyad ${ORIG_DIR}/transpose_matrix_vector_kernel_mod.F90 -oad ${AD_DIR}/ad_transpose_matrix_vector_kernel_mod.F90 -a lhs lhs_e x_e x

clean:
	rm -f ${AD_DIR}/ad*.F90




# Kernels that require no modification (as tl == ad)
# enforce_bc_kernel_mod.F90

# Kernels that need tweaks before psyad can process them

#The DotProduct2CodeTrans transformation only supports the transformation of a dotproduct intrinsic if its arguments are plain arrays
# psyad tl_kinetic_energy_gradient_kernel_mod.F90

# psyad tl_pressure_gradient_bd_kernel_mod.F90
# psyad tl_vorticity_advection_kernel_mod.F90

# Error: no array information for recip_epsilon : psyad tl_moist_dyn_gas_kernel_mod.F90 -oad ad_moist_dyn_gas_kernel_mod.F90 -a moist_dyn_gas mr_v mr_v_at_dof


# Not in previous list
# psyad transpose_matrix_kernel_mod.F90

# Does not exist
# tl_rhs_eos_kernel_mod

# ????
# inc_x_plus_by
