# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Top-level Makefile to test all of the LFRic hands-on tutorials.
.PHONY: transform compile run clean allclean

transform compile run clean allclean:
	${MAKE} -C building_code $@
	${MAKE} -C distributed_memory $@
	${MAKE} -C single_node $@
