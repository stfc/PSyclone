# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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

MPI?=no

ifeq ($(MPI), yes)
	# -P suppresses linemarkers
	FPP?=cpp -P -D_MPI
	# We need to compile with mpif90 if MPI is enabled
	F90?=mpif90
else
	FPP?=cpp -P
endif

PSYROOT=../../../..

# Set it to 'netcdf' or 'binary' to select a different output format
TYPE?=ascii

include ../../lfric_common.mk

ifeq ($(TYPE), netcdf)
	GENERATED_FILES += main-update.nc main-init.nc
else ifeq ($(TYPE), ascii)
	GENERATED_FILES += main-update.ascii main-init.ascii
else
	GENERATED_FILES += main-update.binary main-init.binary
endif
GENERATED_FILES += driver-main-init driver-main-init.F90       \
				   driver-main-update driver-main-update.F90   \
				   *.o *.mod $(EXEC) main_alg.f90 main_psy.f90 \
				   main.x90

F90 ?= gfortran
F90FLAGS ?= -Wall -g -ffree-line-length-none

OBJ = main_psy.o main_alg.o testkern_w0_kernel_mod.o dummy_mod.o

ifeq ($(TYPE), netcdf)
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/netcdf/lfric
	F90FLAGS += $$(nf-config --fflags)
else ifeq ($(TYPE), ascii)
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/ascii/lfric
else
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/binary/lfric
endif

# Even standalone now needs NetCDF because of the LFRic infrastructure
LDFLAGS += $$(nf-config --flibs) $$(nc-config --libs)

EXEC = extract.$(TYPE)
EXTRACT_NAME ?= _extract
EXTRACT_LIB = $(EXTRACT_DIR)/lib$(EXTRACT_NAME).a

F90FLAGS += -I$(EXTRACT_DIR)

transform: extract_process main_psy.f90 main_alg.f90
	
compile: transform $(EXEC)

run: compile
	# Run application and extract data into files
	./$(EXEC)
	# Compile drivers
	make driver-main-init
	make driver-main-update
	# Run drivers with extracted data
	./driver-main-init
	./driver-main-update

$(EXEC): $(LFRIC_LIB) $(EXTRACT_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) $(OBJ) -o $(EXEC) -L$(EXTRACT_DIR) -l$(EXTRACT_NAME) \
	      -L$(LFRIC_PATH) -l$(LFRIC_NAME) $(LDFLAGS)

$(EXTRACT_LIB): $(LFRIC_LIB)
	$(MAKE) -C $(EXTRACT_DIR)

extract_process:
	$(MAKE) -C $(EXTRACT_DIR) process

# Dependencies
main_psy.o:	testkern_w0_kernel_mod.o $(EXTRACT_LIB) $(LFRIC_LIB)
main_alg.o:	main_psy.o
testkern_w0_kernel_mod.o: dummy_mod.o $(LFRIC_LIB)

driver-main-update:	driver-main-update.o
	$(F90) $(F90FLAGS)  driver-main-update.o $(LDFLAGS) -o driver-main-update

driver-main-init:	LFRIC_INCLUDE_FLAGS += -I $(EXTRACT_DIR)/..
driver-main-init:	driver-main-init.o
	$(F90) $(F90FLAGS)  driver-main-init.o $(LDFLAGS) -o driver-main-init

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# Keep the generated psy and alg files
.precious: main.x90 main_psy.f90 main_alg.f90

# This dependency will make sure that read_kernel_data_mod was created
# (which will be inlined in the driver).
main_psy.f90: extract_process
main_alg.f90: main_psy.f90

%.x90:	%.X90
	$(FPP) $< >$@

%_psy.f90:	%.x90
	${PSYCLONE} -api lfric -s ./extract_transform.py       \
	-d . -d $(EXTRACT_DIR)                                 \
	-d $(LFRIC_PATH)                                       \
	-nodm -opsy $*_psy.f90 -oalg $*_alg.f90 $<

allclean: clean
	$(MAKE) -C $(LFRIC_PATH) allclean
	$(MAKE) -C $(EXTRACT_DIR) allclean
	# Remove the read-kernel-data object files:
	$(MAKE) -C $(EXTRACT_DIR)/.. allclean
