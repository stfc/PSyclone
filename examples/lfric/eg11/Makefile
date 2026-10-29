# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# The compiler to use may be specified via the F90 environment variable
#
# export F90=gfortran
# export F90FLAGS="-g -O0"

include ../lfric_common.mk

GENERATED_FILES = helmholtz_solver_alg_psy.f90 helmholtz_solver_alg_mod.f90 *.o *.mod $(EXEC)

F90 ?= gfortran
F90FLAGS ?= -g -O0

OBJ = helmholtz_solver_alg_psy.o helmholtz_solver_alg_mod.o scaled_matrix_vector_kernel_mod.o driver.o

EXEC = will_not_run.exe

# This example will not run as no mesh is set up and the data is not
# initialised in the algorithm layer. The example was simply designed
# to check that the generated code would compile.
run: compile
	@echo "Running not supported for lfric/eg11"

compile: transform $(EXEC)

transform: helmholtz_solver_alg_psy.f90 helmholtz_solver_alg_mod.f90

helmholtz_solver_alg_psy.f90 helmholtz_solver_alg_mod.f90: helmholtz_solver_alg_mod.x90
	${PSYCLONE} -api lfric -s ./async_script.py -opsy helmholtz_solver_alg_psy.f90 -oalg helmholtz_solver_alg_mod.f90 $<

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) $(OBJ) -o $(EXEC) $(LDFLAGS)

# Dependencies
driver.o:			helmholtz_solver_alg_mod.o
helmholtz_solver_alg_psy.o:	scaled_matrix_vector_kernel_mod.o
helmholtz_solver_alg_mod.o:	helmholtz_solver_alg_psy.o
scaled_matrix_vector_kernel_mod.o: $(LFRIC_LIB)

allclean: clean
	make -C $(LFRIC_PATH) allclean
