# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# The compiler to use may be specified via the F90 environment variable
#
# export F90=gfortran
# export F90FLAGS="-g -O0"

include ../lfric_common.mk

OBJ = testkern_mod.o single_invoke_psy.o single_invoke_alg.o

GENERATED_FILES = single_invoke_alg.f90 single_invoke_psy.f90 \
	*.mod ${OBJ} *.exe

F90 ?= gfortran
F90FLAGS ?= -g -O0

.PHONY: transform compile run

# The '-d ../code' argument specifies that PSyclone should search the
# '../code' directory when looking for user-supplied kernels.
transform: single_invoke_alg.f90
	@echo "With distributed-memory support:"
	${PSYCLONE} -api lfric -d ../code ./single_invoke.x90

single_invoke_alg.f90 single_invoke_psy.f90: single_invoke.x90
	@echo "Sequential code:"
	${PSYCLONE} -api lfric -d ../code -nodm ./single_invoke.x90 \
		-oalg single_invoke_alg.f90 \
		-opsy single_invoke_psy.f90

# Note that we compile and link primarily for testing purposes - the
# resulting binary won't run successfully because the example omits
# all the necessary LFRic setup.
compile: $(LFRIC_LIB) single_invoke_psy.o single_invoke_alg.o
	$(F90) $(F90FLAGS) -o will_not_run.exe ${OBJ} ${LDFLAGS}

testkern_mod.o: ../code/testkern_mod.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

single_invoke_alg.o: single_invoke_psy.o
single_invoke_psy.o: testkern_mod.o
testkern_mod.o: $(LFRIC_LIB)

run: compile
	@echo "No run targets for lfric/eg1"
