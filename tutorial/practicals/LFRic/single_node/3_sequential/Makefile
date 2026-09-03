# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../../../common.mk

transform:
	# commented out until issue #1015 is addressed
	# $(PSYCLONE) -s ./loop_fuse.py example_alg.x90 -oalg /dev/null -opsy /dev/null
	# $(PSYCLONE) -s ./kernel_constants.py ../code/helmholtz_solver_alg_mod.x90 -oalg /dev/null -opsy /dev/null
	# $(PSYCLONE) -s ./matvec_opt.py ../code/helmholtz_solver_alg_mod.x90 -oalg /dev/null -opsy /dev/null

compile: transform
	@echo "No compilation supported for tutorial/lfric/single_node/3_sequential"

run: compile
	@echo "No run targets for tutorial/lfric/single_node/3_sequential"

clean:
	rm -f apply_variable_hx_kernel_*_mod.f90 enforce_bc_kernel_*_mod.f90 \
              scaled_matrix_vector_kernel_*_mod.f90 psy.f90

allclean: clean
