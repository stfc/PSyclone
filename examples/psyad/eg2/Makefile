# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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

PSYROOT=../../..
include ${PSYROOT}/examples/common.mk

CONFIG_ENV = PSYCLONE_CONFIG=${PSYCLONE_DIR}/config/psyclone.cfg

F90FLAGS += -ffree-line-length-none

# The names of 'active' variables in the supplied kernel. Must include those
# local variables that are deduced to be active because of their dependence
# on those routine arguments that are active. Note, one of the variables
# (res_dot_product) did not exist in the original code and is a by-product
# of psyad replacing a dot_product intrinsic with equivalent code. This and
# all other local variables will not need to be specified on the command line
# once issue #1556 has been addressed.
ACTIVE_VAR_LIST ?= r_u exner theta moist_dyn_gas moist_dyn_tot moist_dyn_fac grad_term theta_v_e theta_v_at_quad grad_theta_v_at_quad exner_e exner_at_quad res_dot_product
# PSyAD arguments that specify which (if any) of the arguments described in
# the kernel metadata contain geometric information that must not be modified
# by the test harness. The default kernel for this Makefile
# (tl_hydrostatic_kernel) doesn't have any.
GEOMETRY_VAR_LIST ?=
TL_KERNEL_NAME ?= tl_hydrostatic_kernel
TL_KERNEL_FILE = ${TL_KERNEL_NAME}_mod.F90
ADJ_KERNEL_F90_FILE = $(subst tl_,adj_,${TL_KERNEL_FILE})
ADJ_KERNEL_X90_FILE = $(subst .F90,.x90,${ADJ_KERNEL_F90_FILE})

TL_PROC_NAME = $(subst _kernel,_code,${TL_KERNEL_NAME})
ADJ_PROC_NAME = $(subst tl_,adj_,${TL_PROC_NAME})
TL_KERNEL_TYPE = ${TL_KERNEL_NAME}_type
ADJ_KERNEL_TYPE = $(subst tl_,adj_,${TL_KERNEL_TYPE})

# The name of the file that will contain the generated test harness.
HARNESS_X90_FILE = adjoint_test_mod.x90
HARNESS_F90_FILE = adjoint_test_mod.F90

GENERATED_FILES += adj_*.[fF]90 test_harness.exe \
                   *.x90 ${HARNESS_F90_FILE} psy.f90 *.mod *.o

transform: psy.f90

# Process the generated test-harness algorithm with PSyclone.
psy.f90 ${HARNESS_F90_FILE}: ${HARNESS_X90_FILE} ${ADJ_KERNEL_F90_FILE}
	${PSYCLONE} -l all -api dynamo0.3 -nodm -opsy psy.f90 -oalg ${HARNESS_F90_FILE} ${HARNESS_X90_FILE}

# Create the adjoint kernel and the corresponding test
# harness in separate files.
${ADJ_KERNEL_X90_FILE} ${HARNESS_X90_FILE}: ${TL_KERNEL_FILE}
	$(PSYAD) -api dynamo0.3 $< -otest ${HARNESS_X90_FILE} -oad ${ADJ_KERNEL_X90_FILE} ${GEOMETRY_VAR_LIST} -a ${ACTIVE_VAR_LIST}

# Currently PSyAD does not update the kernel metadata (issue #1453)
# so we do that manually here. We have to allow for the case where
# the kernel contains multiple implementations (for mixed precision).
#
# WARNING: the metadata may still be incorrect as we do not change
# the ACCESS properties of the arguments.
# TODO #1772.
${ADJ_KERNEL_F90_FILE}: ${ADJ_KERNEL_X90_FILE}
	sed -e 's/PROCEDURE, NOPASS :: ${TL_PROC_NAME}/PROCEDURE, NOPASS :: ${ADJ_PROC_NAME}/' -e 's/:: ${TL_KERNEL_TYPE}/:: ${ADJ_KERNEL_TYPE}/' \
-e 's/interface ${TL_PROC_NAME}/interface ${ADJ_PROC_NAME}/' \
-e 's/module procedure ${TL_PROC_NAME}.*\w/&_adj/' \
-e 's/public :: ${TL_PROC_NAME}$$/public :: ${ADJ_PROC_NAME}/' $< > $@

compile:
	@echo No compilation supported: the generated Algorithm must be called from within an LFRic mini-app.

run: compile
