# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

.PHONY: vanilla redundant

transform: vanilla redundant

vanilla:
	${PSYCLONE} -api lfric helmholtz_solver_alg_mod.x90

redundant:
	${PSYCLONE} -api lfric -s ./redundant_script.py helmholtz_solver_alg_mod.x90

compile: transform
	@echo "No compilation supported for lfric/eg8"

run: compile
	@echo "No run targets for lfric/eg8"
