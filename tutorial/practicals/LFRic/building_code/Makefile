# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Top-level Makefile for all solutions of the LFRic hands-on tutorials on
# building code.

# Targets
# compile (default): complete build to run examples
# transform: PSyclone target for continuous integration
# clean: Remove generated and compiled code
.DEFAULT_GOAL := compile

.PHONY: transform compile run clean allclean

transform compile run clean allclean:
	${MAKE} -C 1_simple_kernels/part1/solutions $@
	${MAKE} -C 1_simple_kernels/part2/solutions $@
	${MAKE} -C 2_built_ins/solutions $@
	${MAKE} -C 3_time_evolution/solutions $@
	${MAKE} -C 4_psydata $@
