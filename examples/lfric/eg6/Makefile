# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

GENERATED_FILES += dag.png dag.svg dag

.PHONY:  serial dm openmp hybrid reprod

transform: serial dm openmp hybrid reprod

serial:
	${PSYCLONE} -api lfric -nodm alg.x90

dm:
	${PSYCLONE} -api lfric alg.x90

openmp:
	${PSYCLONE} -api lfric -nodm -s ./omp_script.py alg.x90

hybrid:
	${PSYCLONE} -api lfric -s ./omp_script.py alg.x90

reprod:
	${PSYCLONE} -api lfric -s ./omp_reprod_script.py alg.x90

compile: transform
	@echo "No compilation supported for lfric/eg6"

run: compile
	@echo "No run targets for lfric/eg6"
