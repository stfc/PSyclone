# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

.PHONY: serial dm

transform: serial dm backends

serial:
	${PSYCLONE} -api lfric -nodm solver_mod.x90

dm:
	${PSYCLONE} -api lfric solver_mod.x90

compile: transform
	@echo "No compilation supported for lfric/eg4"

backends:
	${PSYCLONE} -api lfric -s ./backends_transform.py -nodm solver_mod.x90

run: compile
	@echo "No run targets for lfric/eg4"
