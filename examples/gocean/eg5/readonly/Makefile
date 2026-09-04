# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# This Makefile picks up the compiler to use plus any flags from
# environment variables. e.g.:
#     export F90=mpif90
#     export F90FLAGS="-O3"
# The dl_esm_inf infrastructure library:
#     export INF_DIR=../../../external/dl_esm_inf/finite_difference/src
# The read-only wrapper library:
#     export READ_ONLY_DIR = ../../../lib/read_only/dl_esm_inf

PSYROOT=../../../..
include $(PSYROOT)/examples/common.mk

GENERATED_FILES += *.o *.mod read_only_test alg.f90 psy.f90

# Location of the infrastructure code (which is a submodule of the
# PSyclone git repo).
SHARED_DIR ?= $(PSYROOT)/external
INF_DIR ?= $(SHARED_DIR)/dl_esm_inf/finite_difference
INF_INC = $(INF_DIR)/src
INF_LIB = $(INF_DIR)/src/lib_fd.a
READ_ONLY_DIR = $(PSYROOT)/lib/read_only/dl_esm_inf
F90FLAGS += -I$(INF_INC) -I $(READ_ONLY_DIR) 
LIB_NAME = lib_read_only.a

# The two kernels used in the application.
KERNELS = init_field_mod.o update_field_mod.o

# The name of the executable
NAME = read_only_test

compile: transform $(NAME)

run: compile
	./read_only_test 2>&1 | \
		grep "2d Double array b_fld%data has been modified in main : update"

alg.f90 psy.f90: transform

transform: test.x90 read_only_transform.py
	${PSYCLONE} -nodm -api "gocean" -s ./read_only_transform.py \
				-opsy psy.f90 -oalg alg.f90 test.x90

$(NAME): $(INF_LIB) $(READ_ONLY_DIR)/$(LIB_NAME) $(KERNELS) alg.o psy.o
	$(F90) $(F90FLAGS) $(KERNELS) alg.o psy.o -o $(NAME) \
	-L$(READ_ONLY_DIR) -l_read_only $(INF_LIB)

# The read-only library
$(READ_ONLY_DIR)/$(LIB_NAME):
	@echo "Making dl_esm_inf readonly wrapper library"
	@echo "=========================================="
	$(MAKE) -C $(READ_ONLY_DIR) F90=$(F90)
	@echo "Finished making dl_esm_inf readonly wrapper library"
	@echo "==================================================="
	@echo

# The dl_esm_inf library
$(INF_LIB):
	@echo "Making dl_esm_inf library"
	@echo "========================="
	$(MAKE) -C $(INF_DIR) F90=$(F90)
	@echo "Finished making dl_esm_inf library"
	@echo "=================================="
	@echo

# Kernel compilation needs the .mod files from dl_esm_inf
$(KERNELS): $(INF_LIB)

alg.o:	psy.o

psy.o:	$(KERNELS)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

allclean: clean
	$(MAKE) -C $(READ_ONLY_DIR) allclean
	# No allclean target for infrastructure library, so use clean
	$(MAKE) -C $(INF_DIR) clean
