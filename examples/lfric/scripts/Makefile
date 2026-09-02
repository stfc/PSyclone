# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

# Get all psyclone scripts in this folder, which are all python files excluding
# the compare_output.py script
SCRIPTS = $(filter-out compare_output.py, $(wildcard *.py))

# The kernel-extraction script generates driver source files.
GENERATED_FILES += driver-*.F90

transform: ${SCRIPTS}

.PHONY: ${SCRIPTS}

${SCRIPTS}:
	LFRIC_OFFLOAD_DIRECTIVES=acc ${PSYCLONE} -api lfric -s ./$@ ../eg3/solver_mod.x90 -oalg /dev/null -opsy /dev/null

compile: transform
	@echo "No compilation supported for lfric/scripts examples"

run: compile
	@echo "No run targets for lfric/scripts examples"
