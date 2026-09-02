# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
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
# The value-range-check wrapper library:
#     export VALUE_RANGE_CHECK_DIR=../../../lib/value_range_check/dl_esm_inf

PSYROOT=../../../..
include $(PSYROOT)/examples/common.mk

GENERATED_FILES += *.o *.mod value_range_check alg.f90 psy.f90

# Location of the infrastructure code (which is a submodule of the
# PSyclone git repo).
SHARED_DIR ?= $(PSYROOT)/external
INF_DIR ?= $(SHARED_DIR)/dl_esm_inf/finite_difference
INF_INC = $(INF_DIR)/src
INF_LIB = $(INF_DIR)/src/lib_fd.a
VALUE_RANGE_CHECK_DIR = $(PSYROOT)/lib/value_range_check/dl_esm_inf
# Don't add the include flags to F90FLAGS, that might cause problems
# when other makefiles are called (which will also use F90FLAGS)
F90_INC_FLAGS= -I$(INF_INC) -I $(VALUE_RANGE_CHECK_DIR)
LIB_NAME = lib_value_range_check.a

# The two kernels used in the application.
KERNELS = init_field_mod.o update_field_mod.o

# The name of the executable
NAME = value_range_check

.PHONY: default $(VALUE_RANGE_CHECK_DIR)/$(LIB_NAME) $(INF_LIB)

compile: transform $(NAME)

alg.f90 psy.f90: transform

transform: test.x90 value_range_check_transformation.py
	${PSYCLONE} -nodm -api "gocean" -s ./value_range_check_transformation.py\
				-opsy psy.f90 -oalg alg.f90 test.x90

run: compile
	# Note that if the output line is not found, the make will abort with error
	./value_range_check 2>&1| grep "PSyData: Variable 'a_fld' has the invalid value"

$(NAME): $(INF_LIB) $(VALUE_RANGE_CHECK_DIR)/$(LIB_NAME) $(KERNELS) alg.o psy.o
	$(F90) $(F90FLAGS) $(KERNELS) alg.o psy.o -o $(NAME) \
	-L$(VALUE_RANGE_CHECK_DIR) -l_value_range_check $(INF_LIB)

# Value range check needs inf lib (otherwise in parallel re-compilation
# inf_lib and value range might be compiled in parallel)
$(VALUE_RANGE_CHECK_DIR)/$(LIB_NAME): $(INF_LIB)

# The value_range_check library
$(VALUE_RANGE_CHECK_DIR)/$(LIB_NAME):
	@echo "Making dl_esm_inf value-range-check wrapper library"
	@echo "==================================================="
	$(MAKE) -C $(VALUE_RANGE_CHECK_DIR) F90=$(F90)
	@echo "Finished making dl_esm_inf value-range-check wrapper library"
	@echo "============================================================"
	@echo

# The dl_esm_inf library
$(INF_LIB):
	@echo "Making dl_esm_inf library"
	@echo "========================="
	$(MAKE) -C $(INF_DIR)
	@echo "Finished making dl_esm_inf library"
	@echo "=================================="
	@echo

# Kernel compilation needs the .mod files from dl_esm_inf
$(KERNELS): $(INF_LIB)

alg.o:	psy.o

psy.o:	$(KERNELS)

%.o: %.f90
	$(F90) $(F90FLAGS) $(F90_INC_FLAGS) -c $<

allclean: clean
	$(MAKE) -C $(VALUE_RANGE_CHECK_DIR) allclean
	$(MAKE) -C $(INF_DIR) allclean
