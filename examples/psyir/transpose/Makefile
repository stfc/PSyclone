# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

F90 ?= gfortran
F90FLAGS ?= -O2 -fopenmp

.PHONY: all
all: run

.PHONY: clean
clean:
	rm -f trans trans_tiled trans_omp trans_omp_tiled
	rm -f trans_tiled.F90 trans_omp.F90 trans_omp_tiled.F90

allclean: clean

transform: trans_tiled.F90 trans_omp.F90 trans_omp_tiled.F90

compile: trans trans_tiled trans_omp trans_omp_tiled

run: compile
	./trans
	./trans_tiled
	./trans_omp
	./trans_omp_tiled

trans_tiled.F90: trans.F90 tile.py
	psyclone -s tile.py trans.F90 -o trans_tiled.F90

trans_omp.F90: trans.F90 omp.py
	psyclone -s omp.py trans.F90 -o trans_omp.F90

trans_omp_tiled.F90: trans.F90 omp-tile.py
	psyclone -s omp-tile.py trans.F90 -o trans_omp_tiled.F90

%: %.F90
	$(F90) $(F90FLAGS) $< -o $@

notebook:
	@echo "No compilation supported for lfric/matmul"
