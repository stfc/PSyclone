# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Top-level Makefile for all LFRic hands-on tutorials on shared memory

.PHONY: transform compile run clean allclean

transform compile run clean allclean:
	${MAKE} -C 1_openmp $@
	${MAKE} -C 2_openacc $@
	${MAKE} -C 3_sequential $@
