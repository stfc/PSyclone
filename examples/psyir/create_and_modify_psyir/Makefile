# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

CONFIG_ENV = PSYCLONE_CONFIG=${PSYCLONE_DIR}/config/psyclone.cfg

transform:
	$(CONFIG_ENV) ${PYTHON} create.py
	$(CONFIG_ENV) ${PYTHON} create_structure_types.py
	$(CONFIG_ENV) ${PYTHON} modify.py

compile:
	@echo "No compilation supported for the PSyIR examples"

run: compile
	@echo "No run targets for PSyIR creation examples"

