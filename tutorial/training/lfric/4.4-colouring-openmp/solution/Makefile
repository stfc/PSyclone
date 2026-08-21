# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Set to omp_colour_transformation.py for the working solution
#SCRIPT= ./omp_colour_transformation.py
SCRIPT ?= ./omp_transformation.py

API=lfric
OBJ = main_alg_psy.o main_alg.o summation_w0_to_w0_kernel_mod.o
EXE = example

include ../../common.mk

# Various targets
# ---------------
.PHONY: transform compile test_run clean allclean test

# Default target: run PSyclone (even if the output file should already exist)
transform:
	$(PSYCLONE) -s $(SCRIPT) -dm -l output \
		     -opsy main_alg_psy.f90 -oalg main_alg.f90 main_alg.x90

compile: $(EXE)

# If we want to run the binary, use the script that adds colour transformation:
test_run: SCRIPT= ./omp_colour_transformation.py

test_run: compile
	@# If OMP_NUM_THREADS is not define, set it to 3:
	OMP_NUM_THREADS=$${OMP_NUM_THREADS:-3} ./example

$(EXE): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXE) $(LDFLAGS)  $$(nf-config --flibs)

# Only required in case the optional colouring transformation is used
clean:
	rm -f $(EXE) $(OBJ) *.mod main_*alg.f90 main_*psy.f90

allclean: clean
	make -C $(LFRIC_PATH) allclean

# Keep the generated psy and alg files
.precious: main_alg_psy.f90 main_alg.f90

# This rule invokes the same PSyclone command, but it won't run if
# the output file already exist, which is useful to avoid recompilation.
main_alg_psy.f90: main_alg.x90 $(SCRIPT)
	$(PSYCLONE) -s $(SCRIPT) -nodm -l output \
		     -opsy main_alg_psy.f90 -oalg main_alg.f90 main_alg.x90

# For PSyclone CI:
test:
	$(MAKE) transform 2>&1 | grep " The kernel has an argument with INC "\
	"access but the loop is of type ''. Colouring is required." >/dev/null
	$(MAKE) clean
	$(MAKE) SCRIPT=./omp_colour_transformation.py transform


# Dependencies
# ------------
main_alg.f90: main_alg_psy.f90
main_alg_psy.o:	summation_w0_to_w0_kernel_mod.o
main_alg.o:	main_alg_psy.o
summation_w0_to_w0_kernel_mod.o: $(LFRIC_LIB)

# File creation rules
# -------------------
%.o: %.f90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<
