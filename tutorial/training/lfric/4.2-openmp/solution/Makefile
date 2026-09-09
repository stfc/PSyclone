# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# The compiler to use may be specified via the F90 environment
#
# export F90=gfortran
# export F90FLAGS="-Wall -g -fcheck=bound"
F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

OBJ = main_alg_psy.o main_alg.o summation_w0_to_w3_kernel_mod.o

EXE = example
API = lfric
include ../../common.mk

# Path of the included simplified LFRic infrastructure files:

# Various targets
# ---------------
.PHONY: transform compile run clean allclean test

# Default target: run PSyclone
transform: main_alg_psy.f90

# This rule invokes the same PSyclone command, but it won't get run if
# the output file already exist, which is useful for compilation to avoid
# recompilation.
main_alg_psy.f90: main_alg.x90 openmp.py  summation_w0_to_w3_kernel_mod.f90
	$(PSYCLONE) -s ./openmp.py -opsy main_alg_psy.f90 -oalg main_alg.f90 main_alg.x90

compile: main_alg_psy.f90 $(EXE)

test_run: compile
	./example

$(EXE): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXE) $(LFRIC_LIB) $$(nf-config --flibs)

clean:
	rm -f $(EXE) $(OBJ) *.mod main_*alg.f90 main_*psy.f90

allclean: clean
	make -C $(LFRIC_PATH) allclean

# Keep the generated psy and alg files
.precious: main_alg_psy.f90 main_alg.f90

# For PSyclone CI:
test: transform

# Dependencies
# ------------
main_alg.f90: main_alg_psy.f90
main_alg_psy.o:	summation_w0_to_w3_kernel_mod.o
main_alg.o:	main_alg_psy.o
summation_w0_to_w3_kernel_mod.o: $(LFRIC_LIB)

# File creation rules
# -------------------
%.o: %.f90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<
