# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

transform:
	${PSYCLONE} -s ./openmp_cpu_levels_trans.py ../code/tra_adv.F90
	${PSYCLONE} -s ./openmp_gpu_levels_trans.py ../code/tra_adv.F90
	PARLLEL_DIRECTIVES="omp_threading" ${PSYCLONE} -s ../scripts/insert_loop_parallelism.py ../code/tra_adv.F90
	PARLLEL_DIRECTIVES="omp_offloading" ${PSYCLONE} -s ../scripts/insert_loop_parallelism.py ../code/tra_adv.F90

compile: transform
	@echo "No compilation supported for nemo/eg1"

run: compile
	@echo "No run targets for nemo/eg1"
