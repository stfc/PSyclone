# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../common.mk

OBJ = main_alg_psy.o main_alg.o summation_w0_to_w3_kernel_mod.o

EXEC = example

# Various targets
# ---------------

# Default target: run PSyclone (even if the output file should already exist)
transform:
	$(PSYCLONE) -s ./omp_transformation.py -dm -l output \
		     -opsy main_alg_psy.f90 -oalg main_alg.f90 main_alg.x90

compile: $(EXEC)

run: compile
	@# Turning the examples to work with MPI would required adding Yaxt
	@# as dependency. So, the examples are all compiled without MPI support.
	@echo =============================================
	@echo Warning: this example does not run as MPI job
	@echo It would just start individual, unconnected
	@echo processes \(since it does not call MPI_Init,
	@echo nor does it setup a domain decomposition\).
	@echo =============================================
	./$(EXEC)

test: transform

test_run: compile
	$(MAKE) run | grep " minmax of field1"

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) -L$(LFRIC_PATH) -l$(LFRIC_NAME) \
		   $$(nf-config --flibs)

clean:
	rm -f $(EXEC) $(OBJ) *.mod main_*alg.f90 main_*psy.f90

allclean: clean
	make -C $(LFRIC_PATH) allclean

# Keep the generated psy and alg files
.precious: main_alg_psy.f90 main_alg.f90

# This rule invokes the same PSyclone command, but it won't run if
# the output file already exist, which is useful to avoid recompilation.
main_alg_psy.f90: main_alg.x90
	$(PSYCLONE) -s ./omp_transformation.py -dm -l output -opsy main_alg_psy.f90 -oalg main_alg.f90 main_alg.x90

# For PSyclone CI:
test: transform

# Dependencies
# ------------
main_alg.f90: main_alg_psy.f90
main_alg_psy.o:	summation_w0_to_w3_kernel_mod.o
main_alg.o:	main_alg_psy.o
summation_w0_to_w3_kernel_mod.o: $(LFRIC_LIB)
