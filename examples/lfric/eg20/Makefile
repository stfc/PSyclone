# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../lfric_common.mk

GENERATED_FILES = test_alg_mod_psy.f90 testkern_mod.F90 test_alg_mod.[fx]90 *.o *.mod

OBJECTS = testkern_mod.o test_alg_mod_psy.o test_alg_mod.o driver.o

run: compile
	@echo "The compiled code for lfric/eg20 is not executable."

# Although we compile and link, the resulting binary will not execute (because the
# driver does not do the necessary setup).
compile: transform ${LFRIC_LIB} ${OBJECTS}
	$(F90) $(F90FLAGS) -o will_not_run.exe ${OBJECTS} ${LDFLAGS}

# Dependencies
%.F90:
	ln -s ../code/$@ .

# Keep the generated psy and alg files
.precious: test_alg_mod_psy.f90 test_alg_mod.f90

# Use the psyclone-kern tool to create an algorithm layer that will
# 'invoke' our kernel
test_alg_mod.x90: testkern_mod.F90
	${KERNEL_ALG_GEN} -api lfric -o test_alg_mod.x90 ../code/testkern_mod.F90

transform: test_alg_mod_psy.f90

# Process the generated algorithm layer with PSyclone to generate a compilable
# algorithm file and a suitable PSy-layer subroutine.
test_alg_mod_psy.f90 test_alg_mod.f90: test_alg_mod.x90
	${PSYCLONE} -api lfric -d ../code -opsy test_alg_mod_psy.f90 -oalg test_alg_mod.f90 $<

allclean: clean
	make -C $(LFRIC_PATH) allclean

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<
