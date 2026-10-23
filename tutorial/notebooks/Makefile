# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Makefile that exercises all Jupyter notebooks (for use in continuous
# integration).

include ../common.mk

.DEFAULT_GOAL := notebook

notebook clean:
	${MAKE} -C fparser2 $@
	${MAKE} -C psyir $@
	${MAKE} -C nemo $@
