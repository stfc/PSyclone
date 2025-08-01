# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council
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
# Modified: A. R. Porter, STFC Daresbury Laboratory

# This Makefile picks up the compiler to use plus any flags from
# environment variables. e.g.:
#     export F90=mpif90
#     export F90FLAGS="-O3"
# The dl_esm_inf infrastructure library:
#     export INF_DIR=../../../../external/dl_esm_inf/finite_difference/src
# The dl_esm_inf extract wrapper library:
#     export EXTRACT_DIR = ../../../../lib/extract/netcdf/dl_esm_inf

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
include $(PSYROOT)/examples/common.mk

# Set it to 'netcdf' to use the NetCDF writer
TYPE?=binary

GENERATED_FILES += *.o *.mod $(NAME)  alg.f90 psy.f90   \
		  $(DRIVER_INIT).$(TYPE)   $(DRIVER_INIT).f90   \
		  $(DRIVER_UPDATE).$(TYPE) $(DRIVER_UPDATE).f90 \
		  main.x90

# Location of the infrastucture code (which is a submodule of the
# PSyclone git repo).
SHARED_DIR ?= $(PSYROOT)/external
INF_DIR ?= $(SHARED_DIR)/dl_esm_inf/finite_difference
INF_INC = $(INF_DIR)/src
INF_LIB = $(INF_DIR)/src/lib_fd.a
ifeq ($(TYPE), netcdf)
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/netcdf/dl_esm_inf
	F90FLAGS += $$(nf-config --fflags)
	LDFLAGS += $$(nf-config --flibs) $$(nc-config --libs)
	GENERATED_FILES += main-init.nc main-update.nc
else ifeq ($(TYPE), ascii)
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/ascii/dl_esm_inf
	GENERATED_FILES += main-init.ascii main-update.ascii
else
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/binary/dl_esm_inf
	GENERATED_FILES += main-init.binary main-update.binary

endif

LIB_NAME = lib_extract.a


# The two kernels used in the application.
KERNELS = init_field_mod.o update_field_mod.o

# The name of the executable
NAME = extract_test.$(TYPE)

# The name of the stand-alone driver that will call the init kernel.
DRIVER_INIT = driver-main-init

# The name of the stand-alone driver that will call the update kernel.
DRIVER_UPDATE = driver-main-update

.PHONY: transform compile run

run: compile
	./$(NAME)
	./driver-main-init.$(TYPE)
	./driver-main-update.$(TYPE)

compile: transform $(NAME) $(DRIVER_INIT).$(TYPE) $(DRIVER_UPDATE).$(TYPE)

transform: psy.f90


F90FLAGS += -I$(INF_INC) -I$(EXTRACT_DIR)

alg.f90 psy.f90: test.x90 extract_transform.py
	$(PSYCLONE) -nodm -api "gocean" -s ./extract_transform.py\
				-opsy psy.f90 -oalg alg.f90 test.x90

$(NAME): $(INF_LIB) $(EXTRACT_DIR)/$(LIB_NAME) $(KERNELS) alg.o psy.o
	$(F90) $(F90FLAGS) $(KERNELS) alg.o psy.o -o $(NAME) \
	$(EXTRACT_DIR)/$(LIB_NAME) $(INF_LIB) $(LDFLAGS)

#TODO #1757: $(INF_LIB) is required because of the meta-data in the
#            kernel - once this is fixed, $(INF_LIB) can be removed.
$(DRIVER_INIT).$(TYPE):	$(KERNELS) $(DRIVER_INIT).o
	$(F90) $(F90FLAGS) $(KERNELS) $(DRIVER_INIT).o -o $(DRIVER_INIT).$(TYPE) \
		$(INF_LIB) $(EXTRACT_DIR)/$(LIB_NAME) $(LDFLAGS)

#TODO #1757: $(INF_LIB) is required because of the meta-data in the
#            kernel - once this is fixed, $(INF_LIB) can be removed.
$(DRIVER_UPDATE).$(TYPE): $(KERNELS) $(DRIVER_UPDATE).o
	$(F90) $(F90FLAGS) $(KERNELS) $(DRIVER_UPDATE).o -o $(DRIVER_UPDATE).$(TYPE) \
		$(INF_LIB) $(EXTRACT_DIR)/$(LIB_NAME) $(LDFLAGS)

# The dl_esm_inf library
$(INF_LIB):
	$(MAKE) -C $(INF_DIR) F90=$(F90)

$(EXTRACT_DIR)/$(LIB_NAME):
	$(MAKE) -C $(EXTRACT_DIR)

alg.o:	psy.o

psy.o:	$(KERNELS)

# The driver will be created when creating the psy.f90 file.
# This dependency is required, otherwise a 'make compile' in a clean
# directory will fail.
$(DRIVER_INIT).f90: psy.f90
$(DRIVER_UPDATE).f90: psy.f90

# Dependency to INF_LIB to make sure the mod file are available
$(KERNELS):  $(INF_LIB)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.x90: %.X90
	$(FPP) $< >$@

# This target requires that the netcdf (Fortran) development package be
# installed
$(EXTRACT_DIR)/lib_kernel_data_netcdf.a:
	make -C $(EXTRACT_DIR)

allclean: clean
	make -C $(INF_DIR) clean
	make -C $(EXTRACT_DIR) clean
