# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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

GENERATED_FILES = ${OUT_DIR}/adj*.[fF]90

single: ${ADJ_KERNEL_FILE}

${ADJ_KERNEL_FILE}: ${TL_KERNEL_FILE}
	$(PSYAD) -api dynamo0.3 $< -oad ${OUT_DIR}/$@ -a ${ACTIVE_VAR_LIST}

transform:
	make single -e TL_KERNEL_NAME=transpose_matrix_vector_kernel ACTIVE_VAR_LIST="lhs lhs_e x_e x"
	make single -e TL_KERNEL_NAME=matrix_vector_kernel ACTIVE_VAR_LIST="lhs x lhs_e x_e"
	make single -e TL_KERNEL_NAME=dg_inc_matrix_vector_kernel ACTIVE_VAR_LIST="lhs x lhs_e x_e"
	make single -e TL_KERNEL_NAME=tl_hydrostatic_kernel ACTIVE_VAR_LIST="r_u exner theta grad_term exner_at_quad theta_v_at_quad res_dot_product grad_theta_v_at_quad theta_v_e exner_e moist_dyn_tot moist_dyn_gas"
	make single -e TL_KERNEL_NAME=tl_moist_dyn_mass_kernel ACTIVE_VAR_LIST="moist_dyn_tot mr_v mr_cl mr_r mr_ci mr_s mr_g mr_v_at_dof mr_cl_at_dof mr_r_at_dof mr_ci_at_dof mr_s_at_dof mr_g_at_dof"
	make single -e TL_KERNEL_NAME=tl_pressure_gradient_bd_kernel ACTIVE_VAR_LIST="r_u_bd exner pressure_gradient_bd_e bdary_term exner_av theta_v_at_fquad theta_v_e exner_e exner_next_e theta moist_dyn_tot moist_dyn_gas"
	make single -e TL_KERNEL_NAME=tl_rhs_project_eos_kernel ACTIVE_VAR_LIST="rhs_eos exner rho theta moist_dyn_gas eos exner_quad theta_vd_quad rho_quad rho_e exner_e theta_vd_e"
	make single -e TL_KERNEL_NAME=tl_rhs_sample_eos_kernel ACTIVE_VAR_LIST="rhs_eos exner rho theta moist_dyn_gas exner_cell theta_vd_cell rho_cell rho_e exner_e theta_vd_e"
	# Correct active list below, as "advective_increment m3_inv" also works?
	make single -e TL_KERNEL_NAME=w3_advective_update_kernel ACTIVE_VAR_LIST="advective_increment u v w wind"
	make single -e TL_KERNEL_NAME=tl_poly_advective_kernel ACTIVE_VAR_LIST="advective u v dtdx dtdy tracer wind"
	make single -e TL_KERNEL_NAME=combine_w2_field_kernel ACTIVE_VAR_LIST="uvw w uv"
	make single -e TL_KERNEL_NAME=split_w2_field_kernel ACTIVE_VAR_LIST="uvw w uv"

compile:
	@echo No compilation supported.

run: compile
