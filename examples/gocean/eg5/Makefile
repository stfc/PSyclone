# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

EXAMPLES=extract value_range_check omp_profile profile readonly

include ../../top_level.mk

# Compiling/running omp_profile will compile infrastructure etc with
# OpenMP flags. This can later trigger compilation failures with other
# examples which do NOT provide the OpenMP flags. Therefore, do an
# allclean before and after compiling/running the omp_profile example.
run_omp_profile:
	make -C omp_profile allclean
	make -C omp_profile run
	make -C omp_profile allclean

compile_omp_profile:
	make -C omp_profile allclean
	make -C omp_profile compile
	make -C omp_profile allclean
