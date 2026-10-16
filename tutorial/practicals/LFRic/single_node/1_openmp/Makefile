# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../../../common.mk

transform:
	$(PSYCLONE) --psykal-dsl lfric -oalg /dev/null -opsy /dev/null -s ./omp_script.py ../code/helmholtz_solver_alg_mod.x90
	psyclone    --psykal-dsl lfric -oalg /dev/null -opsy /dev/null -s ./omp_script.py ../code/helmholtz_solver_alg_mod.x90 --config psyclone.cfg
	$(PSYCLONE) --psykal-dsl lfric -oalg /dev/null -opsy /dev/null -s ./omp_script.py helmholtz_solver_alg_mod.x90 -d ../code
	$(PSYCLONE) --psykal-dsl lfric -oalg /dev/null -opsy /dev/null -s solutions/omp_script.py ../code/helmholtz_solver_alg_mod.x90
	$(PSYCLONE) --psykal-dsl lfric -oalg /dev/null -opsy /dev/null -s solutions/omp_script.py helmholtz_solver_alg_mod.x90 -d ../code

compile: transform
	@echo "No compilation supported for tutorial/lfric/single_node/1_openmp"

run: compile
	@echo "No run targets for tutorial/lfric/single_node/1_openmp"

clean:
	rm -f psy.f90

allclean: clean
