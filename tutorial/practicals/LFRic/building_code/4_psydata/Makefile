# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# We want to avoid that a user accidentally starts this Makefile, since then
# unexpected transformations might be applied. So we disable the default
# target here and ask the user to select a different Makefile.
default:
	@echo "Please use one of the transformation-specific Makefiles, e.g."
	@echo "make -f Makefile.extract_one"

PSYDATA_PATH?=$(PSYCLONE_RELPATH)/lib/value_range_check/lfric
PSYDATA_LIB_NAME?=_value_range_check

compile:
	echo "Compilation for building_code/4_psydata not yet working."

# Target for testing the tutorials with compilation (use only one PSyData wrapper)
# TODO #3114
#compile:
#	make -f solutions/Makefile.value_range_check

run: compile
	./time_evolution

# Target for CI, only do the transformation (use all transformation scripts)
transform:
	make clean
	make -f solutions/Makefile.extract_one time_evolution_alg_mod_psy.f90
	make clean
	make -f solutions/Makefile.extract_all time_evolution_alg_mod_psy.f90
	make clean
	make -f solutions/Makefile.value_range_check time_evolution_alg_mod_psy.f90
	make clean
	make -f solutions/Makefile.readonly_all time_evolution_alg_mod_psy.f90

# This will define a 'transform' target, that tutorial checking will use
include Makefile.inc
