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

TL_KERNEL_FILE = ${TL_KERNEL_NAME}_mod_tweaked.F90
ADJ_KERNEL_FILE = $(subst _tweaked,,$(addprefix adj_,$(subst tl_,,${TL_KERNEL_FILE})))

GENERATED_FILES = ${OUT_DIR}/adj*.[fF]90

single: ${ADJ_KERNEL_FILE}

${ADJ_KERNEL_FILE}: ${TL_KERNEL_FILE}
	$(PSYAD) -api dynamo0.3 $< -oad ${OUT_DIR}/$@ -a ${ACTIVE_VAR_LIST}

transform:
	make single -e TL_KERNEL_NAME=tl_kinetic_energy_gradient_kernel ACTIVE_VAR_LIST="r_u ru_e ke_at_quad res_dot_product mul2 u_at_quad u_e u"
	make single -e TL_KERNEL_NAME=tl_moist_dyn_gas_kernel ACTIVE_VAR_LIST="moist_dyn_gas mr_v mr_v_at_dof"
	make single -e TL_KERNEL_NAME=tl_project_eos_pressure_kernel ACTIVE_VAR_LIST="exner rho exner_e r_exner exner_at_quad rho_at_quad rho_e tmp2 theta theta_vd_e theta_vd_at_quad moist_dyn_gas"
	make single -e TL_KERNEL_NAME=tl_vorticity_advection_kernel ACTIVE_VAR_LIST="r_u res_dot_product vorticity_term cross_product1 cross_product2 j_vorticity u_at_quad mul2 vorticity_at_quad wind vorticity"

compile:
	@echo No compilation supported.

run: compile
