# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

.PHONY: vanilla openmp

transform: vanilla openmp

vanilla:
	${PSYCLONE} -api lfric advective_inc_alg_mod.x90

openmp:
	${PSYCLONE} -api lfric -s ./colouring_and_omp.py -nodm advective_inc_alg_mod.x90

compile: transform
	@echo "No compilation supported for lfric/eg9"

run: compile
	@echo "No run targets for lfric/eg9"
