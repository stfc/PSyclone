# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# ------------------------------------------------------------------------------
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Laboratory

PSYROOT=../../../..
include ${PSYROOT}/examples/common.mk

CONFIG_ENV = PSYCLONE_CONFIG=${PSYCLONE_DIR}/config/psyclone.cfg

F90FLAGS += -ffree-line-length-none

OUT_DIR ?= ../adjoint_partial
ACTIVE_VAR_LIST ?= 
TL_KERNEL_NAME ?=

TL_KERNEL_FILE = ${TL_KERNEL_NAME}_mod.F90
ADJ_KERNEL_FILE = $(addprefix adj_,$(subst tl_,,${TL_KERNEL_FILE}))

GENERATED_FILES = ${OUT_DIR}/adj*.[fF]90 ../adjoint/adj_transpose_matrix_vector_kernel_mod.F90 ../adjoint/adj_hydrostatic_kernel_mod.F90 ../adjoint/adj_moist_dyn_mass_kernel_mod.F90 ../adjoint/adj_w3_advective_update_kernel_mod.F90 ../adjoint/adj_combine_w2_field_kernel_mod.F90 ../adjoint/adj_split_w2_field_kernel_mod.F90 ../adjoint/adj_sample_flux_kernel_mod.F90 ../adjoint/adj_tracer_viscosity_kernel_mod.F90 ../adjoint/adj_dg_matrix_vector_kernel_mod.F90 ../adjoint/adj_w2_to_w1_projection_kernel_mod.F90

single: ${ADJ_KERNEL_FILE}

${ADJ_KERNEL_FILE}: ${TL_KERNEL_FILE}
	$(PSYAD) -api dynamo0.3 $< -oad ${OUT_DIR}/$@ -a ${ACTIVE_VAR_LIST}

transform:
	# The following files need modifying after being processed by
	# PSyAD so go into the adjoint_partial directory.
	$(MAKE) single -e TL_KERNEL_NAME=tl_pressure_gradient_bd_kernel ACTIVE_VAR_LIST="r_u_bd exner pressure_gradient_bd_e bdary_term exner_av theta_v_at_fquad theta_v_e exner_e exner_next_e theta moist_dyn_tot moist_dyn_gas"
	$(MAKE) single -e TL_KERNEL_NAME=tl_rhs_sample_eos_kernel ACTIVE_VAR_LIST="rhs_eos exner rho theta moist_dyn_gas exner_cell theta_vd_cell rho_cell rho_e exner_e theta_vd_e"
	$(MAKE) single -e TL_KERNEL_NAME=tl_poly_advective_kernel ACTIVE_VAR_LIST="advective u v dtdx dtdy tracer wind"
	$(MAKE) single -e TL_KERNEL_NAME=strong_curl_kernel ACTIVE_VAR_LIST="xi res_dot_product curl_u u"

	# The following files no not need any modification after PSyAD processing so
	# go straight into the adjoint directory.
	$(MAKE) single -e TL_KERNEL_NAME=transpose_matrix_vector_kernel ACTIVE_VAR_LIST="lhs lhs_e x_e x" OUT_DIR="../adjoint"
	$(MAKE) single -e TL_KERNEL_NAME=tl_hydrostatic_kernel ACTIVE_VAR_LIST="r_u exner theta grad_term exner_at_quad theta_v_at_quad res_dot_product grad_theta_v_at_quad theta_v_e exner_e moist_dyn_tot moist_dyn_gas" OUT_DIR="../adjoint"
	$(MAKE) single -e TL_KERNEL_NAME=tl_moist_dyn_mass_kernel ACTIVE_VAR_LIST="moist_dyn_tot mr_v mr_cl mr_r mr_ci mr_s mr_g mr_v_at_dof mr_cl_at_dof mr_r_at_dof mr_ci_at_dof mr_s_at_dof mr_g_at_dof" OUT_DIR="../adjoint"
	# Correct active list below, as "advective_increment m3_inv" also works?
	$(MAKE) single -e TL_KERNEL_NAME=w3_advective_update_kernel ACTIVE_VAR_LIST="advective_increment u v w wind" OUT_DIR="../adjoint"
	$(MAKE) single -e TL_KERNEL_NAME=combine_w2_field_kernel ACTIVE_VAR_LIST="uvw w uv" OUT_DIR="../adjoint"
	$(MAKE) single -e TL_KERNEL_NAME=split_w2_field_kernel ACTIVE_VAR_LIST="uvw w uv" OUT_DIR="../adjoint"
	$(MAKE) single -e TL_KERNEL_NAME=sample_flux_kernel ACTIVE_VAR_LIST="flux u" OUT_DIR="../adjoint"
	#
	# The arguments to helmholtz_operator_kernel do not conform to
	# those expected by the metadata. I presume this is a
	# psykalite kernel. This causes psyad to fail when updating
	# the adjoint metadata description. It can be seen that the
	# arguments differ by looking at the number of arguments at
	# the start of the argument list. Two are expected (cell and
	# nlayers) but a load of stencil information is passed first.
	#
	# $(MAKE) single -e TL_KERNEL_NAME=helmholtz_operator_kernel ACTIVE_VAR_LIST="helm_c f_op helm_dd helm_uu helm_d helm_u helm_s helm_n helm_w helm_e d_op ec_op m3_exner_star p3theta compound_div"
	$(MAKE) single -e TL_KERNEL_NAME=tracer_viscosity_kernel ACTIVE_VAR_LIST="theta_inc viscosity_mu" OUT_DIR="../adjoint"
	$(MAKE) single -e TL_KERNEL_NAME=dg_matrix_vector_kernel ACTIVE_VAR_LIST="lhs lhs_e x x_e" OUT_DIR="../adjoint"
	$(MAKE) single -e TL_KERNEL_NAME=w2_to_w1_projection_kernel ACTIVE_VAR_LIST="v_w1 u_w2 vu res_dot_product wind" OUT_DIR="../adjoint"

compile:
	@echo No compilation supported.

run: compile

allclean:
	rm -f $(GENERATED_FILES)
