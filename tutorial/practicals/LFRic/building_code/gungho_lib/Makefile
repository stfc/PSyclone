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
# Author: I. Kavcic, Met Office
# Modified J. Henrichs, Bureau of Meteorology

F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

# LFRic infrastructure library
PSYCLONE_RELPATH = ../../../../..
LFRIC_PATH = $(PSYCLONE_RELPATH)/src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_NAME = lfric_netcdf
LFRIC_LIB = $(LFRIC_PATH)/lib$(LFRIC_NAME).a

# This will add the required include flags to LFRIC_INCLUDE_FLAGS
include $(LFRIC_PATH)/lfric_include_flags.inc

# Source rules
ALGORITHMS = $(wildcard *_alg_mod.x90)
ALGORITHM_SRC = $(ALGORITHMS:.x90=.f90)
PSY_SRC = $(ALGORITHM_SRC:.f90=_psy.f90)
GHLIB_SRC = $(wildcard *.F90) $(wildcard *.f90)

# PSyclone command-line options
PSYCLONE_CMD = -nodm -l all

# Object rules
ALGORITHM_OBJ = $(filter %.o,$(ALGORITHM_SRC:.f90=.o))
PSY_OBJ = $(filter %.o,$(PSY_SRC:.f90=.o))
GHLIB_OBJ = $(filter %.o,$(GHLIB_SRC:.F90=.o) $(GHLIB_SRC:.f90=.o)) \
	 $(ALGORITHM_OBJ) $(PSY_OBJ)

default: $(LFRIC_LIB) $(GHLIB_OBJ)
	$(AR) $(ARFLAGS) libgungho.a $(GHLIB_OBJ)
default: $(LIB)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH) netcdf

# Dependencies
# ------------
assign_coordinate_field_mod.o: base_mesh_config_mod.o planet_config_mod.o
configuration_mod.o: base_mesh_config_mod.o extrusion_uniform_config_mod.o \
	finite_element_config_mod.o io_utility_mod.o partitioning_config_mod.o \
	perturbation_bell_config_mod.o planet_config_mod.o timestepping_config_mod.o \
	write_methods_mod.o
write_diagnostics_alg_mod.o: write_methods_mod.o write_diagnostics_alg_mod_psy.o

%.o: %.F90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<

$(ALGORITHM_SRC): $(PSY_SRC)

# Keep the generated Alg and PSy files
.precious: $(ALGORITHM_SRC) $(PSY_SRC)

%_psy.f90: %.x90
	psyclone $(PSYCLONE_CMD) --config $(PSYCLONE_RELPATH)/config/psyclone.cfg \
	-opsy $*_psy.f90 -oalg $*.f90 $<

clean:
	rm -f *.o *.mod *.a

allclean: clean
	$(MAKE) -C $(LFRIC_PATH) allclean
