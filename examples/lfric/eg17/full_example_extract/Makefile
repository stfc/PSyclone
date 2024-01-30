# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Modifications: A. R. Porter, STFC Daresbury Laboratory

# The compiler to use may be specified via the F90 environment
#
# export F90=gfortran
# export F90FLAGS="-Wall -g -fcheck=bound"

PSYROOT=../../../..

# Set it to 'netcdf' to use the NetCDF writer
TYPE?=standalone

include $(PSYROOT)/examples/common.mk

ifeq ($(TYPE), netcdf)
	GENERATED_FILES += main-update.nc main-init.nc
else
	GENERATED_FILES += main-update.binary main-init.binary
endif
GENERATED_FILES += driver-main-init driver-main-init.F90       \
				   driver-main-update driver-main-update.F90   \
				   *.o *.mod $(EXEC) main_alg.f90 main_psy.f90

F90 ?= gfortran
F90FLAGS ?= -Wall -g -ffree-line-length-none

OBJ = main_psy.o main_alg.o testkern_w0_kernel_mod.o

ifeq ($(TYPE), netcdf)
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/netcdf/lfric
	LDFLAGS += $$(nf-config --flibs)
	F90FLAGS += $$(nf-config --fflags)
else
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/standalone/lfric
endif

EXEC = extract.$(TYPE)
EXTRACT_NAME ?= _extract
EXTRACT_LIB = $(EXTRACT_DIR)/lib$(EXTRACT_NAME).a
READ_KERNEL_DATA_OBJ = $(EXTRACT_DIR)/../read_kernel_data_mod.o
LFRIC_PATH ?= $(PSYROOT)/src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_PATH)/lib$(LFRIC_NAME).a

# This will setup the required include flags in LFRIC_INCLUDE_FLAGS
include $(LFRIC_PATH)/lfric_include_flags.inc

LFRIC_INCLUDE_FLAGS += -I$(EXTRACT_DIR)

transform: main_psy.f90 main_alg.f90

compile: transform $(EXEC)

run: compile
	./$(EXEC)

$(EXEC): $(LFRIC_LIB) $(EXTRACT_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) $(OBJ) -o $(EXEC) -L$(EXTRACT_DIR) -l$(EXTRACT_NAME) \
	      -L$(LFRIC_PATH) -l$(LFRIC_NAME) $(LDFLAGS)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

$(EXTRACT_LIB): $(LFRIC_LIB)
	$(MAKE) -C $(EXTRACT_DIR)

# Dependencies
main_psy.o:	testkern_w0_kernel_mod.o $(EXTRACT_LIB) $(LFRIC_LIB)
main_alg.o:	main_psy.o
testkern_w0_kernel_mod.o: $(LFRIC_LIB)

driver-main-update:	LFRIC_INCLUDE_FLAGS += -I $(EXTRACT_DIR)/..
driver-main-update:	driver-main-update.o $(READ_KERNEL_DATA_OBJ)
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS)  driver-main-update.o \
		   $(LDFLAGS) -o driver-main-update

driver-main-init:	LFRIC_INCLUDE_FLAGS += -I $(EXTRACT_DIR)/..
driver-main-init:	driver-main-init.o $(READ_KERNEL_DATA_OBJ)
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS)  driver-main-init.o \
		   $(LDFLAGS) -o driver-main-init

driver-main-update.o driver-main-init.o:  $(READ_KERNEL_DATA_OBJ)

%.o: %.F90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<

# Keep the generated psy and alg files
.precious: main_psy.f90 main_alg.f90

main_alg.f90: main_psy.f90

$(READ_KERNEL_DATA_OBJ):
	$(MAKE) -C $(EXTRACT_DIR)/..

%_psy.f90:	%.x90
	${PSYCLONE} -s ./extract_transform.py       \
	-d . -d $(EXTRACT_DIR) -d $(EXTRACT_DIR)/.. \
	-d $(LFRIC_PATH)                            \
	-nodm -opsy $*_psy.f90 -oalg $*_alg.f90 $<

allclean: clean
	$(MAKE) -C $(LFRIC_PATH) allclean
	$(MAKE) -C $(EXTRACT_DIR) allclean
	# Remove the read-kernel-data object files:
	$(MAKE) -C $(EXTRACT_DIR)/.. allclean
