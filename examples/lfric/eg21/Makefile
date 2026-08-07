# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../lfric_common.mk

ALG_NAME = nlayers_ndata_eg_alg

OBJ = testkern_nlayers_ndata_mod.o ${ALG_NAME}_psy.o ${ALG_NAME}.o
GENERATED_FILES = ${ALG_NAME}.f90 ${ALG_NAME}_psy.f90 \
    testkern_nlayers_ndata_mod.F90 *.o \
    *.mod will_not_run.exe

# Although we link an executable, the example is too simplistic to
# actually execute.
run: compile
	@echo "lfric/eg21 is not executable."

# Dependencies
%.F90:
	ln -s ../code/$@ .

# Keep the generated psy and alg files
.precious: ${ALG_NAME}_psy.f90 ${ALG_NAME}.f90

transform: ${ALG_NAME}_psy.f90

# Process the algorithm layer with PSyclone to generate a compilable
# algorithm file and a suitable PSy-layer subroutine.
${ALG_NAME}_psy.f90 ${ALG_NAME}.f90: ${ALG_NAME}.x90
	${PSYCLONE} -api lfric -d ../code -opsy ${ALG_NAME}_psy.f90 -oalg ${ALG_NAME}.f90 $<

# Compile and link purely to test correctness - the resulting executable
# won't actually work.
compile: transform ${LFRIC_LIB} ${OBJ}
	${F90} ${F90FLAGS} -o will_not_run.exe ${OBJ} ${LDFLAGS}

allclean: clean
	make -C $(LFRIC_PATH) allclean
