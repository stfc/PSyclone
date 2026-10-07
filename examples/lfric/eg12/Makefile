# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

.PHONY: extract extract_transformed find_kernel

transform: extract extract_transformed find_kernel

extract:
	${PSYCLONE} -api lfric -nodm -s ./extract_nodes.py ../code/gw_mixed_schur_preconditioner_alg_mod.x90

extract_transformed:
	${PSYCLONE} -api lfric -nodm -s ./extract_kernel_with_transformations.py \
  ../code/gw_mixed_schur_preconditioner_alg_mod.x90

find_kernel:
	PSYCLONE_CONFIG=${PSYCLONE_DIR}/config/psyclone.cfg ${PYTHON} find_kernel.py

compile: transform
	@echo "No compilation supported for lfric/eg12"

run: compile
	@echo "No run targets for lfric/eg12"
