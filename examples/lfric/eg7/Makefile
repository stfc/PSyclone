# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

transform:
	${PSYCLONE} -api lfric alg.x90

compile: transform
	@echo "No compilation supported for lfric/eg7"

run: compile
	@echo "No run targets for lfric/eg7"
