# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

# LFRic infrastructure library
PSYCLONE_RELPATH = ../../../../..
LFRIC_PATH = $(PSYCLONE_RELPATH)/external/lfric_infrastructure/src
LFRIC_NAME = lfric
LFRIC_LIB = $(LFRIC_PATH)/lib$(LFRIC_NAME).a

# This will add the required include flags to LFRIC_INCLUDE_FLAGS
include $(LFRIC_PATH)/lfric_include_flags.mk

# Source rules
ALGORITHMS = $(wildcard *_alg_mod.x90)
ALGORITHM_SRC = $(ALGORITHMS:.x90=.f90)
PSY_SRC = $(ALGORITHM_SRC:.f90=_psy.f90)
GHLIB_SRC = $(wildcard *.F90) $(wildcard *.f90)

# PSyclone command-line options
PSYCLONE_CMD = --psykal-dsl lfric -nodm -l all

# Object rules
ALGORITHM_OBJ = $(filter %.o,$(ALGORITHM_SRC:.f90=.o))
PSY_OBJ = $(filter %.o,$(PSY_SRC:.f90=.o))
GHLIB_OBJ = $(filter %.o,$(GHLIB_SRC:.F90=.o) $(GHLIB_SRC:.f90=.o)) \
	 $(ALGORITHM_OBJ) $(PSY_OBJ)

# TODO #3114
default:
	echo "Compilation for building_code/1_simple_kernels/part1 not working yet."

default: $(LFRIC_LIB) $(GHLIB_OBJ)
	$(AR) $(ARFLAGS) libgungho.a $(GHLIB_OBJ)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

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
