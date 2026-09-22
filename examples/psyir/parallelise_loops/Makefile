# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

GENERATED_FILES= output.f90

transform:
	${PSYCLONE} -s ./add_parallelism.py example.f90 -o output.f90

compile: transform
	@echo "No compilation supported for psyir/parallelise_loops"

run: compile
	@echo "No run targets for psyir/parallelise_loops"
