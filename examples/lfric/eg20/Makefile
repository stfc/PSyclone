# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../lfric_common.mk

GENERATED_FILES = test_alg_mod_psy.f90 testkern_mod.F90 test_alg_mod.[fx]90

run: compile

compile: transform ${LFRIC_LIB}
	@echo "#1770 Compilation is not supported: the generated code must be "
	@echo "inserted into an LFRic mini-app."

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
