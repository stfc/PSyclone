# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../lfric_common.mk

GENERATED_FILES = mixed_precision_psy.f90 alg.f90 ${EXEC} ${OBJ} *.mod

F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

OBJ = alg.o mixed_precision_psy.o mixed_kernel_mod.o

EXEC = will_not_run.exe

.PHONY: transform compile run

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) $(LDFLAGS)

compile: transform $(EXEC)

# Runs PSyclone to do the code generation. Also demonstrates the use of the
# example 'kernel_print' transformation which prints the Fortran of each
# kernel found.
transform:
	${PSYCLONE} -api lfric algorithm.x90 -s ../scripts/kernel_print.py -opsy mixed_precision_psy.f90 -oalg alg.f90

alg.f90 mixed_precision_psy.f90: transform
alg.o: mixed_precision_psy.o
mixed_precision_psy.o: mixed_kernel_mod.o

run: compile
	@echo "No run targets for lfric/eg19"

allclean: clean
	make -C $(LFRIC_PATH) allclean
