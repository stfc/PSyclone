# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Compiler settings
F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

# Source rules
DRIVER_SRC = simple_kernels_driver.f90
ALGORITHMS = $(wildcard *_alg_mod.x90)
ALGORITHM_SRC = $(ALGORITHMS:.x90=.f90)
PSY_SRC = $(ALGORITHM_SRC:.f90=_psy.f90)
KERNEL_SRC = $(wildcard *_kernel_mod.f90)

# PSyclone command-line options
PSYCLONE_CMD = --psykal-dsl lfric -nodm -l all

# Executable
EXEC = ./simple_kernels_part2

# LFRic infrastructure library
PSYCLONE_RELPATH = ../../../../../..
LFRIC_PATH ?= $(PSYCLONE_RELPATH)/external/lfric_infrastructure/src
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_PATH)/lib$(LFRIC_NAME).a

# This will add the required include flags to LFRIC_INCLUDE_FLAGS
include $(LFRIC_PATH)/lfric_include_flags.mk

# Object rules
DRIVER_OBJ = $(filter %.o,$(DRIVER_SRC:.f90=.o))
ALGORITHM_OBJ = $(filter %.o,$(ALGORITHM_SRC:.f90=.o))
PSY_OBJ = $(filter %.o,$(PSY_SRC:.f90=.o))
KERNEL_OBJ = $(filter %.o,$(KERNEL_SRC:.f90=.o))
EXAMPLE_OBJ = $(DRIVER_OBJ) $(ALGORITHM_OBJ) $(PSY_OBJ) $(KERNEL_OBJ)

# Targets
.DEFAULT_GOAL := compile

.PHONY: transform compile run clean allclean

# Test PSyclone target for continuous integration
transform: $(PSY_SRC) $(ALGORITHM_SRC)

compile: transform $(EXEC)

run: compile
	$(EXEC)

$(EXEC): $(LFRIC_LIB) $(EXAMPLE_OBJ)
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) $(EXAMPLE_OBJ) \
	-o $(EXEC) -L$(LFRIC_PATH) -l$(LFRIC_NAME)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

# Dependencies
$(DRIVER_OBJ): $(ALGORITHM_OBJ)
$(ALGORITHM_OBJ): $(PSY_OBJ)
$(PSY_OBJ): $(KERNEL_OBJ)
$(KERNEL_OBJ): $(LFRIC_LIB)

%.o %.mod: %.F90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<

%.o %.mod: %.f90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<

$(ALGORITHM_SRC): $(PSY_SRC)

# Keep the generated Alg and PSy files
.precious: $(ALGORITHM_SRC) $(PSY_SRC)

%_psy.f90: %.x90
	psyclone $(PSYCLONE_CMD) --config $(PSYCLONE_RELPATH)/config/psyclone.cfg \
	-opsy $*_psy.f90 -oalg $*.f90 $<

clean:
	rm -f *.o *.mod $(EXEC) $(ALGORITHM_SRC) $(PSY_SRC)

allclean: clean
	make -C $(LFRIC_PATH) clean
