# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

GENERATED_FILES += 
ENV = PSYCLONE_CONFIG=${PSYCLONE_DIR}/config/psyclone.cfg

.PHONY: transform

transform:
	${PSYCLONE} -nodm -s ./backends_transform.py -api gocean alg.f90 \
		-oalg /dev/null -opsy /dev/null --keep-comments
