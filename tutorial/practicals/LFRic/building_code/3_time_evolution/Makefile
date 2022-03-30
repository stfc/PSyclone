# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# Author: I. Kavcic, Met Office
# Modified by D. Sergeev, University of Exeter

# Compiler settings
F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

# Source rules
DRIVER_SRC = time_evolution_driver.f90
ALGORITHMS = $(wildcard *_alg_mod.x90)
ALGORITHM_SRC = $(ALGORITHMS:.x90=.f90)
PSY_SRC = $(ALGORITHM_SRC:.f90=_psy.f90)
KERNEL_SRC = $(wildcard *_kernel_mod.f90)

# PSyclone command-line options
PSYCLONE_CMD = -nodm -l all

# Executable
EXEC = time_evolution

# LFRic infrastructure library
PSYCLONE_RELPATH = ../../../../..
LFRIC_PATH = $(PSYCLONE_RELPATH)/src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_NAME = lfric_netcdf
LFRIC_LIB = $(LFRIC_PATH)/lib$(LFRIC_NAME).a
F90FLAGS += $$(nf-config --fflags) -I$(LFRIC_PATH)

# GungHo auxiliary libraries
GHLIB_PATH = ../gungho_lib
GHLIB_NAME = gungho
GHLIB_LIB = $(GHLIB_PATH)/lib$(GHLIB_NAME).a
F90FLAGS += -I$(GHLIB_PATH)

# Object rules
DRIVER_OBJ = $(filter %.o,$(DRIVER_SRC:.f90=.o))
ALGORITHM_OBJ = $(filter %.o,$(ALGORITHM_SRC:.f90=.o))
PSY_OBJ = $(filter %.o,$(PSY_SRC:.f90=.o))
KERNEL_OBJ = $(filter %.o,$(KERNEL_SRC:.F90=.o) $(KERNEL_SRC:.f90=.o))
EXAMPLE_OBJ = $(DRIVER_OBJ) $(ALGORITHM_OBJ) $(PSY_OBJ) $(KERNEL_OBJ)

# Targets
.DEFAULT_GOAL := compile

.PHONY: compile test clean

compile: test $(EXEC)

$(EXEC): $(LFRIC_LIB) $(GHLIB_LIB) $(EXAMPLE_OBJ)
	$(F90) $(F90FLAGS) $(EXAMPLE_OBJ) -o $(EXEC) -L$(LFRIC_PATH) \
	-l$(LFRIC_NAME) $$(nf-config --flibs) -L$(GHLIB_PATH) -l$(GHLIB_NAME)

$(GHLIB_LIB):
	$(MAKE) -C $(GHLIB_PATH)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH) netcdf 

# Dependencies
$(DRIVER_OBJ): $(ALGORITHM_OBJ) $(PSY_OBJ) $(KERNEL_OBJ) $(LFRIC_LIB) $(GHLIB_LIB)
$(ALGORITHM_OBJ): $(PSY_OBJ) $(KERNEL_OBJ) $(LFRIC_LIB) $(GHLIB_LIB)
$(PSY_OBJ): $(KERNEL_OBJ) $(LFRIC_LIB) $(GHLIB_LIB)
$(KERNEL_OBJ): $(LFRIC_LIB) $(GHLIB_LIB)

%.o %.mod: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o %.mod: %.f90
	$(F90) $(F90FLAGS) -c $<

$(ALGORITHM_SRC): $(PSY_SRC)

# Keep the generated Alg and PSy files
.precious: $(ALGORITHM_SRC) $(PSY_SRC)

# Test PSyclone target for continuous integration
test: $(PSY_SRC) $(ALGORITHM_SRC)

%_psy.f90: %.x90
	psyclone $(PSYCLONE_CMD) --config $(PSYCLONE_RELPATH)/config/psyclone.cfg \
	-opsy $*_psy.f90 -oalg $*.f90 $<

clean:
	rm -f *.o *.mod *.txt *.png $(EXEC) $(ALGORITHM_SRC) $(PSY_SRC)
	make -C $(GHLIB_PATH) clean
	make -C $(LFRIC_PATH) clean
