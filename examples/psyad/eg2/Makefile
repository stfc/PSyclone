# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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

NAN_TEST_DIR = $(PSYROOT)/lib/nan_test/lfric
LIB_NAME = lib_nan_test.a

CONFIG_ENV = PSYCLONE_CONFIG=${PSYCLONE_DIR}/config/psyclone.cfg
GENERATED_FILES += adj_*.[fF]90 test_harness.exe \
                   *.x90 alg.f90 psy.f90 *.mod *.o

LFRIC_DIR ?= $(PSYROOT)/src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_DIR)/lib$(LFRIC_NAME).a

F90FLAGS += -I$(LFRIC_DIR) -ffree-line-length-none -I$(NAN_TEST_DIR)

ACTIVE_VAR_LIST ?= r_u exner theta moist_dyn_gas moist_dyn_tot moist_dyn_fac grad_term theta_v_e theta_v_at_quad grad_theta_v_at_quad exner_e exner_at_quad res_dot_product
TL_KERNEL_NAME ?= tl_hydrostatic_kernel
TL_KERNEL_FILE = ${TL_KERNEL_NAME}_mod.F90
ADJ_KERNEL_F90_FILE = $(subst tl_,adj_,${TL_KERNEL_FILE})
ADJ_KERNEL_X90_FILE = $(subst .F90,.x90,${ADJ_KERNEL_F90_FILE})

TL_PROC_NAME = $(subst _kernel,_code,${TL_KERNEL_NAME})
ADJ_PROC_NAME = $(subst tl_,adj_,${TL_PROC_NAME})
TL_KERNEL_TYPE = ${TL_KERNEL_NAME}_type
ADJ_KERNEL_TYPE = $(subst tl_,adj_,${TL_KERNEL_TYPE})

transform: psy.f90

psy.f90 alg.f90: main_alg.x90 ${ADJ_KERNEL_F90_FILE}
	${PSYCLONE} -api dynamo0.3 -nodm -opsy psy.f90 -oalg alg.f90 -s ./test_nan_transform.py main_alg.x90

# Create the adjoint kernel and the corresponding test
# harness in separate files. Note, one of the variables provided in the
# psyad command line below (res_dot_product) did not exist in the
# original code and is a by-product of psyad replacing a dot_product
# intrinsic with equivalent code. This and all other local variables
# will not need to be specified on the command line once issue #1556
# has been addressed.
${ADJ_KERNEL_X90_FILE} main_alg.x90: ${TL_KERNEL_FILE}
	$(PSYAD) -api dynamo0.3 $< -otest main_alg.x90 -oad ${ADJ_KERNEL_X90_FILE} -a ${ACTIVE_VAR_LIST}

${ADJ_KERNEL_F90_FILE}: ${ADJ_KERNEL_X90_FILE}
	# Currently PSyAD does not update the kernel metadata (issue #1453)
	# so we do that manually here. We have to allow for the case where
	# the kernel contains multiple implementations (for mixed precision).
	sed -e 's/PROCEDURE, NOPASS :: ${TL_PROC_NAME}/PROCEDURE, NOPASS :: ${ADJ_PROC_NAME}/' -e 's/:: ${TL_KERNEL_TYPE}/:: ${ADJ_KERNEL_TYPE}/' \
-e 's/interface ${TL_PROC_NAME}/interface ${ADJ_PROC_NAME}/' \
-e 's/module procedure ${TL_PROC_NAME}.*\w/&_adj/' \
-e 's/public :: ${TL_PROC_NAME}$$/public :: ${ADJ_PROC_NAME}/' $< > $@

compile: ${ADJ_KERNEL_F90_FILE} alg.f90 psy.f90 ${LFRIC_LIB} test_harness.exe

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_DIR)

# The nan-test library
$(NAN_TEST_DIR)/$(LIB_NAME):
	@echo "Making LFRic nan-test wrapper library"
	@echo "=========================================="
	$(MAKE) -C $(NAN_TEST_DIR) F90=$(F90)
	@echo "Finished making LFRic nan-test wrapper library"
	@echo "==================================================="
	@echo

test_harness.exe: $(subst .F90,.o,${TL_KERNEL_FILE}) $(subst .f90,.o,${ADJ_KERNEL_F90_FILE}) psy.o alg.o $(NAN_TEST_DIR)/$(LIB_NAME) ${LFRIC_LIB}
	$(F90) $(F90FLAGS) -o test_harness.exe $^

psy.o: ${LFRIC_LIB} $(NAN_TEST_DIR)/$(LIB_NAME)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<
%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

run: compile
	./test_harness.exe

allclean: clean
	${MAKE} -C ${LFRIC_DIR} allclean
	${MAKE} -C ${NAN_TEST_DIR} allclean
