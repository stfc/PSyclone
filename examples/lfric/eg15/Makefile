# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

transform:
	${PSYCLONE} -api lfric -s ./matvec_opt.py \
  ../code/gw_mixed_schur_preconditioner_alg_mod.x90 \
  -oalg /dev/null -opsy /dev/null

compile: transform
	@echo "No compilation supported for lfric/eg15"

run: compile
	@echo "No run targets for lfric/eg15"
