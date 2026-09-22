# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk
include ../lfric_common.mk

KERNELS = matrix_vector_kernel_mod.o dg_matrix_vector_kernel_mod.o
SRC_DIR = ../code
GENERATED_FILES = alg.f90 psy.f90 psy.o $(KERNELS) *.mod

transform:
	${PSYCLONE} -api lfric -s ./kernel_constants.py \
  ../code/gw_mixed_schur_preconditioner_alg_mod.x90 \
  -oalg alg.f90 -opsy psy.f90

# We can't fully compile this but we can at least do the PSy layer
compile: transform $(LFRIC_LIB) $(KERNELS)
	$(F90) -c $(F90FLAGS) psy.f90

%.o: $(SRC_DIR)/%.F90
	$(F90) -c $(F90FLAGS) $<

run: compile
	@echo "No run targets for lfric/eg13"
