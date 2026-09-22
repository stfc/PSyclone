# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

.PHONY: vanilla compile run

transform: vanilla

vanilla:
	${PSYCLONE} -api lfric -d ./ -d ../code advection_alg_mod.x90

compile: transform
	@echo "No compilation supported for lfric/eg18"

run: compile
	@echo "No run targets for lfric/eg18"
