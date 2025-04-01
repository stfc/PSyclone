# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# ------------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology

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

# Location of the infrastucture code (which is a submodule of the
# PSyclone git repo).
SHARED_DIR ?= $(PSYROOT)/external
INF_DIR ?= $(SHARED_DIR)/dl_esm_inf/finite_difference
INF_INC = $(INF_DIR)/src
INF_LIB = $(INF_DIR)/src/lib_fd.a
VALUE_RANGE_CHECK_DIR = $(PSYROOT)/lib/value_range_check/dl_esm_inf
F90FLAGS += -I$(INF_INC) -I $(VALUE_RANGE_CHECK_DIR)
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
	./value_range_check 2>&1| grep "PSyData: Variable a_fld has the invalid value"

$(NAME): $(INF_LIB) $(VALUE_RANGE_CHECK_DIR)/$(LIB_NAME) $(KERNELS) alg.o psy.o
	$(F90) $(F90FLAGS) $(KERNELS) alg.o psy.o -o $(NAME) \
	-L$(VALUE_RANGE_CHECK_DIR) -l_value_range_check $(INF_LIB)

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
	$(F90) $(F90FLAGS) -c $<

allclean: clean
	$(MAKE) -C $(VALUE_RANGE_CHECK_DIR) allclean
	$(MAKE) -C $(INF_DIR) allclean
