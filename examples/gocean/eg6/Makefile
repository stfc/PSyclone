#------------------------------------------------------------------------------
# BSD 2-Clause License
# 
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#------------------------------------------------------------------------------
# Author: J. Henrichs, Australian Bureau of Meteorology
# Modified: A. R. Porter, STFC Daresbury Laboratory

include ../../common.mk

# Reset the default goal to be 'all', i.e. to include compilation
.DEFAULT_GOAL:=
DEFAULT: all

# The two kernels used in the application.
KERNELS = init_field_mod.o update_field_mod.o

# The name of the executable
EXEC = extract_test

# The name of the stand-alone driver that will do the init invokes.
DRIVER_INIT = driver-main-init

# The name of the stand-alone driver that will call the update function.
DRIVER_UPDATE = driver-main-update

GENERATED_FILES += *.o *.mod $(EXEC) $(DRIVER_INIT) $(DRIVER_INIT).f90 \
		  $(DRIVER_UPDATE) $(DRIVER_UPDATE).f90 alg.f90 psy.f90

# Location of the infrastucture code (which is a submodule of the
# PSyclone git repo).
SHARED_DIR = ../../../external
INF_DIR = $(SHARED_DIR)/dl_esm_inf/finite_difference
INF_INC = $(INF_DIR)/src
INF_LIB = $(INF_DIR)/src/lib_fd.a

EXTRACT_DIR = ../../../lib/extract/dl_esm_inf/netcdf
F90FLAGS += $$(nf-config --fflags) -I$(INF_INC) -I$(EXTRACT_DIR)
LIB_NAME = lib_kernel_data_netcdf.a

all: transform compile

compile: transform $(EXEC)
	@echo "TODO the driver does not yet compile because of #644"

test:	$(DRIVER_INIT) $(DRIVER_UPDATE)

# The driver does not yet compile because of #644
$(DRIVER_INIT):	$(KERNELS) $(DRIVER_INIT).o main.o \
				$(EXTRACT_DIR)/lib_kernel_data_netcdf.a
	$(F90) $(KERNELS) $(DRIVER_INIT).o main.o \
	-o $(DRIVER_INIT)            \
	-L$(EXTRACT_DIR) -l_kernel_data_netcdf \
	$$(nf-config --flibs)

# The driver does not yet compile because of #644
$(DRIVER_UPDATE): $(KERNELS) $(DRIVER_UPDATE).o main.o \
				$(EXTRACT_DIR)/lib_kernel_data_netcdf.a
	$(F90) $(KERNELS) $(DRIVER_UPDATE).o main.o \
	-o $(DRIVER_UPDATE)                         \
	-L$(EXTRACT_DIR) -l_kernel_data_netcdf      \
	$$(nf-config --flibs)

$(EXEC): $(KERNELS) $(EXTRACT_DIR)/lib_kernel_data_netcdf.a alg.o psy.o
	$(F90)  $(KERNELS) alg.o psy.o -o $(EXEC) \
	-L$(EXTRACT_DIR) -l_kernel_data_netcdf    \
	$(INF_LIB)                                \
	$$(nf-config --flibs)

transform: test.x90 extract_transform.py
	$(PSYCLONE) -nodm -l -api "gocean1.0" -s ./extract_transform.py\
		     -opsy psy.f90 -oalg alg.f90 test.x90

psy.f90 alg.f90: transform

alg.o:	psy.o

psy.o:	$(KERNELS)

# Dependency to INF_LIB to make sure the mod file are available
$(KERNELS):  $(INF_LIB)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# This target requires that the netcdf (Fortran) development package be
# installed
$(EXTRACT_DIR)/lib_kernel_data_netcdf.a:
	make -C $(EXTRACT_DIR)

# The dl_esm_inf library
$(INF_LIB):
	$(MAKE) -C $(INF_DIR) F90=$(F90)

allclean: clean
	make -C $(INF_DIR) clean
	make -C $(EXTRACT_DIR) clean
