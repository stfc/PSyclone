# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2026, Science and Technology Facilities Council.
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
# Modifications: A. R. Porter and S. Siso, STFC Daresbury Laboratory

# The compiler to use may be specified via the F90 environment
#
# export F90=gfortran
# export F90FLAGS="-Wall -g -fcheck=bound"

PSYROOT=../../../..

# Set it to 'netcdf' or 'binary' to select a different output format
TYPE?=ascii

include ../../lfric_common.mk

GENERATED_FILES += *.o *.mod $(EXEC) main_alg.f90 main_psy.f90

F90 ?= gfortran
F90FLAGS ?= -Wall -g -ffree-line-length-none

OBJ = main_psy.o main_alg.o testkern_w0_kernel_mod.o

VALUE_RANGE_DIR ?= $(PSYROOT)/lib/value_range_check/lfric
VALUE_RANGE_NAME = _value_range_check

VALUE_RANGE_LIB=$(VALUE_RANGE_DIR)/lib$(VALUE_RANGE_NAME).a
# Even standalone now needs NetCDF because of the LFRic infrastructure
LDFLAGS += $$(nf-config --flibs) $$(nc-config --libs)

EXEC = value_range_check

F90FLAGS += -I$(VALUE_RANGE_DIR)

transform: main_psy.f90 main_alg.f90
	
compile: transform $(EXEC)

run: compile
	@# Run application and grep fro the expected value range message:
	PSY_VALUE_RANGE="field1_data=0:7" ./$(EXEC) 2>&1 | grep "at index/indices"

$(EXEC): $(LFRIC_LIB) $(VALUE_RANGE_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) $(OBJ) -o $(EXEC) \
		  -L$(VALUE_RANGE_DIR) -l$(VALUE_RANGE_NAME)            \
		  -L$(LFRIC_PATH) -l$(LFRIC_NAME) $(LDFLAGS)

$(VALUE_RANGE_LIB): $(LFRIC_LIB)
	$(MAKE) -C $(VALUE_RANGE_DIR)

value_range_process:
	$(MAKE) -C $(VALUE_RANGE_DIR)

# Dependencies
main_alg.f90: main_psy.f90

main_psy.o:	testkern_w0_kernel_mod.o $(VALUE_RANGE_LIB) $(LFRIC_LIB)
main_alg.o:	main_psy.o
testkern_w0_kernel_mod.o: $(LFRIC_LIB)

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# Keep the generated psy and alg files
.precious: main_psy.f90 main_alg.f90

main_psy.f90:	main.x90
	${PSYCLONE} -api lfric -s ./value_range_check_transformation.py \
	-d . -d $(VALUE_RANGE_DIR)                                      \
	-d $(LFRIC_PATH)                                                \
	-nodm -opsy main_psy.f90 -oalg main_alg.f90 main.x90

allclean: clean
	$(MAKE) -C $(LFRIC_PATH) allclean
	$(MAKE) -C $(VALUE_RANGE_DIR) allclean
	# Remove the read-kernel-data object files:
	$(MAKE) -C $(VALUE_RANGE_DIR)/.. allclean
