# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

.PHONY: vanilla inline loopfuse

transform: vanilla inline loopfuse

# The '-d ../code' argument specifies that PSyclone should search the
# '../code' directory when looking for user-supplied kernels.

# The '-s my_script.py' argument specifies that PSyclone should apply
# the transformation script 'my_script.py.'

vanilla:
	${PSYCLONE} -api lfric -nodm -d ../code \
			-s ./print_psyir_trans.py ./multi_invoke_mod.x90

inline:
	${PSYCLONE} -api lfric -nodm -d ../code \
			-s ./module_inline_trans.py ./multi_invoke_mod.x90

loopfuse:
	${PSYCLONE} -api lfric -nodm -d ../code \
			-s ./loop_fuse_trans.py ./multi_invoke_mod.x90

compile: transform
	@echo "No compilation supported for lfric/eg2"

run: compile
	@echo "No run targets for lfric/eg2"
