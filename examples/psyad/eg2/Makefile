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

CONFIG_ENV = PSYCLONE_CONFIG=${PSYCLONE_DIR}/config/psyclone.cfg
GENERATED_FILES += tl_hydrostatic_kernel_mod_adj.[fF]90 test_harness.exe \
                   main_alg.x90 alg.f90 psy.f90 *.mod *.o

LFRIC_DIR ?= $(PSYROOT)/src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_DIR)/lib$(LFRIC_NAME).a

F90FLAGS += -I$(LFRIC_DIR) -ffree-line-length-none

# The first command will write the adjoint kernel to stdout. The
# second, if supported, would write both it and the corresponding test
# harness to separate files.
transform: tl_hydrostatic_kernel_mod.F90 ../../lfric/eg20/alg_gen.py
	$(PSYAD) tl_hydrostatic_kernel_mod.F90 -a r_u exner theta moist_dyn_gas moist_dyn_tot moist_dyn_fac grad_term theta_v_e theta_v_at_quad grad_theta_v_at_quad exner_e exner_at_quad
	$(PSYAD) tl_hydrostatic_kernel_mod.F90 -otest /dev/null -oad tl_hydrostatic_kernel_mod_adj.F90 -a r_u exner theta moist_dyn_gas moist_dyn_tot moist_dyn_fac grad_term theta_v_e theta_v_at_quad grad_theta_v_at_quad exner_e exner_at_quad
	# Currently PSyAD does not update the kernel metadata (issue #1453)
	# so we do that manually here.
	sed -e 's/PROCEDURE, NOPASS :: tl_hydrostatic_code/PROCEDURE, NOPASS :: tl_hydrostatic_code_adj/' tl_hydrostatic_kernel_mod_adj.F90 > tl_hydrostatic_kernel_mod_adj.f90
	# Ultimately this will use the new `python-kern` command but that is
	# still being developed.
	python ./harness_gen.py tl_hydrostatic_kernel_type ./tl_hydrostatic_kernel_mod.F90
	${PSYCLONE} -api dynamo0.3 -opsy psy.f90 -oalg alg.f90 main_alg.x90

compile: alg.f90 psy.f90 test_harness.exe

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_DIR)

psy.f90 alg.f90 tl_hydrostatic_kernel_mod_adj.f90: transform

test_harness.exe: ${LFRIC_LIB} tl_hydrostatic_kernel_mod.o tl_hydrostatic_kernel_mod_adj.o psy.o alg.o
	$(F90) $(F90FLAGS) -o test_harness.exe tl_hydrostatic_kernel_mod.o \
                  tl_hydrostatic_kernel_mod_adj.o psy.o alg.o ${LFRIC_LIB}

psy.o: ${LFRIC_LIB}

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<
%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

run: compile
	./test_harness.exe

allclean: clean
	${MAKE} -C ${LFRIC_DIR} allclean

