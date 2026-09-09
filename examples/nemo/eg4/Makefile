# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

FORTRAN_FILES = tridiagonal_solve.f90 if_example.f90 copy_stencil.f90 hori_diff.f90

.PHONY: ${FORTRAN_FILES} intrinsic tra_adv

transform: ${FORTRAN_FILES} intrinsic tra_adv

${FORTRAN_FILES}:
	${PSYCLONE} -s ./sir_trans.py $@ -o /dev/null

# intrinsic_example.f90 and tracer_advection.f90 can make use of
# different transformation scripts
intrinsic:
	${PSYCLONE} -s ./sir_trans.py intrinsic_example.f90 -o /dev/null
	${PSYCLONE} -s ./sir_trans_all.py intrinsic_example.f90 -o /dev/null
tra_adv:
	${PSYCLONE} -s ./sir_trans_loop.py tra_adv_compute.F90 -o /dev/null
	${PSYCLONE} -s ./sir_trans_all.py tra_adv_compute.F90 -o /dev/null

# We don't do any compilation for this example
compile: transform
	@echo "No compilation supported for nemo/eg4"

run: compile
	@echo "No run targets for nemo/eg4"
