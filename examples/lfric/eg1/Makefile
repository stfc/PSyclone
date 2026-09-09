# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

.PHONY: transform compile run

# The '-d ../code' argument specifies that PSyclone should search the
# '../code' directory when looking for user-supplied kernels.
transform:
	@echo "Sequential code:"
	${PSYCLONE} -api lfric -d ../code -nodm ./single_invoke.x90
	@echo "With distributed-memory support:"
	${PSYCLONE} -api lfric -d ../code ./single_invoke.x90

compile: transform
	@echo "No compilation supported for lfric/eg1"

run: compile
	@echo "No run targets for lfric/eg1"
