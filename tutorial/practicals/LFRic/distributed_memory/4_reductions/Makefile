# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../../../common.mk

transform:
	$(PSYCLONE) --psykal-dsl lfric -s ./schedule.py -oalg /dev/null -opsy /dev/null helmholtz_solver_alg_mod.x90 -d ../code

compile: transform
	@echo "No compilation supported for tutorial/lfric/distributed_memory/4_reductions"

run: compile
	@echo "No run targets for tutorial/lfric/distributed_memory/4_reductions"

clean:
	@echo "Nothing to clean"

allclean: clean
