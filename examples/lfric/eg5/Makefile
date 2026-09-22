# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../lfric_common.mk

OBJ = conservative_flux_kernel_mod.o subgrid_coeffs_kernel_mod.o jules_kernel_mod.o \
alg_psy.o alg.o

GENERATED_FILES = ${OBJ} alg_psy.f90 alg.f90 *.mod will_not_run.exe

transform: alg_psy.f90

compile: transform ${LFRIC_LIB} ${OBJ}
	${F90} ${F90FLAGS} -o will_not_run.exe ${OBJ} ${LDFLAGS}

run: compile
	@echo "No run targets for lfric/eg5"

alg_psy.f90 alg.f90: alg.x90
	${PSYCLONE} -api lfric -opsy alg_psy.f90 -oalg alg.f90 $<

allclean: clean
	make -C ${LFRIC_PATH} allclean
