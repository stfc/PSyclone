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
	rm -f matmul matmul_tiled matmul_omp matmul_omp_tiled
	rm -f matmul_tiled.F90 matmul_omp.F90 matmul_omp_tiled.F90

allclean: clean

matmul_tiled.F90: matmul.F90 tile.py
	psyclone -s tile.py matmul.F90 -o matmul_tiled.F90

matmul_omp.F90: matmul.F90 omp.py
	psyclone -s omp.py matmul.F90 -o matmul_omp.F90

matmul_omp_tiled.F90: matmul.F90 omp-tile.py
	psyclone -s omp-tile.py matmul.F90 -o matmul_omp_tiled.F90

transform: matmul_tiled.F90 matmul_omp.F90 matmul_omp_tiled.F90

compile: matmul matmul_tiled matmul_omp matmul_omp_tiled

run: compile
	./matmul
	./matmul_tiled
	./matmul_omp
	./matmul_omp_tiled

%: %.F90
	$(F90) $(F90FLAGS) $< -o $@

notebook:
	@echo "No compilation supported for lfric/matmul"
