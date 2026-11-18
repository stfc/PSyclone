# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../lfric_common.mk

EXEC = will_not_run.exe

GENERATED_FILES = $(OBJ) *.mod intergrid_3levels_psy.f90 intergrid_3levels.f90 $(EXEC)

OBJ = intergrid_3levels_psy.o intergrid_3levels.o restrict_kernel_mod.o prolong_kernel_mod.o

.PHONY: transform compile run kernels

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) $(LDFLAGS)

transform intergrid_3levels_psy.f90 intergrid_3levels.f90:
	${PSYCLONE} -api lfric intergrid_3levels.x90 \
		-opsy intergrid_3levels_psy.f90 -oalg intergrid_3levels.f90

compile: transform $(EXEC)

run: compile
	@echo "No run targets for lfric/eg10"

intergrid_3levels_psy.o: prolong_kernel_mod.o restrict_kernel_mod.o
intergrid_3levels.o: intergrid_3levels_psy.o

kernels: prolong_kernel_mod.o restrict_kernel_mod.o

