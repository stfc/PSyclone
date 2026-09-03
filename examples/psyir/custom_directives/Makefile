# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

F90 ?= gfortran
F90FLAGS ?= -O2 -fopenmp

transform:
	psyclone --keep-directives -s identify_custom_directives.py directive_filtering.F90 -o directives_omp.F90

.PHONY: clean
clean:
	rm -f directives_omp
	rm -f directives_omp.F90

directives_omp.F90: directive_filtering.F90 identify_custom_directives.py
	psyclone --keep-directives -s identify_custom_directives.py directive_filtering.F90 -o directives_omp.F90

%: %.F90
	$(F90) $(F90FLAGS) $< -o $@

compile: transform directives_omp identify_custom_directives.py

run: compile
	./directives_omp
