# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

GENERATED_FILES= cpu_output.F90 gpu_output.F90

transform:
	${PSYCLONE} -s ./openmp_cpu_nowait_trans.py ../code/tra_adv.F90 -o cpu_output.F90
	${PSYCLONE} -s ./openmp_gpu_nowait_trans.py ../code/tra_adv.F90 -o gpu_output.F90

compile: transform
	@echo "No compilation supported for nemo/eg7"

run: compile
	@echo "No run targets for nemo/eg7"
