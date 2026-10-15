# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
# once issue #1595 has been addressed.
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
HARNESS_X90_FILE = adjt_hydrostatic_alg_mod.x90
HARNESS_F90_FILE = adjt_hydrostatic_alg_mod.F90

GENERATED_FILES += adj_*.[fF]90 test_harness.exe \
                   *.x90 ${HARNESS_F90_FILE} psy.f90 *.mod *.o

transform: psy.f90

# Process the generated test-harness algorithm with PSyclone.
psy.f90 ${HARNESS_F90_FILE}: ${HARNESS_X90_FILE} ${ADJ_KERNEL_F90_FILE}
	${PSYCLONE} -l all -api lfric -nodm -opsy psy.f90 -oalg ${HARNESS_F90_FILE} ${HARNESS_X90_FILE}

# Create the adjoint kernel and the corresponding test
# harness in separate files.
${ADJ_KERNEL_X90_FILE} ${HARNESS_X90_FILE}: ${TL_KERNEL_FILE}
	$(PSYAD) -api lfric $< -otest ${HARNESS_X90_FILE} -oad ${ADJ_KERNEL_X90_FILE} ${GEOMETRY_VAR_LIST} -a ${ACTIVE_VAR_LIST}

${ADJ_KERNEL_F90_FILE}: ${ADJ_KERNEL_X90_FILE}
	cp $< $@

compile:
	@echo No compilation supported: the generated Algorithm must be called from within an LFRic mini-app.

run: compile
