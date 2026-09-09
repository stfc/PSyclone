# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../../../common.mk

transform:
	# Uses the psyclone.cfg configuration file in this directory
	$(PSYCLONE) --psykal-dsl lfric -s ./schedule.py -oalg /dev/null -opsy /dev/null ../code/helmholtz_solver_alg_mod.x90 --config psyclone.cfg

compile: transform
	@echo "No compilation supported for tutorial/lfric/distributed_memory/2_annexed_dofs"

run: compile
	@echo "No run targets for tutorial/lfric/distributed_memory/2_annexed_dofs"

clean:
	@echo "Nothing to clean"

allclean: clean
