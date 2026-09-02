# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Compiler settings
F90 ?= gfortran
F90FLAGS ?= -Wall -g

# Source rules
SRC = prop_perturbation_kernel_mod.f90 time_evolution_driver.f90  \
	  time_evolution_alg_mod.x90 init_perturbation_kernel_mod.f90

# Object rules
OBJ = $(addsuffix .o,$(basename $(SRC)))
PSY = $(filter %.x90,$(SRC))
# Each .x90 file also creates a _psy.* file:
PSY_SRC = $(addsuffix _psy.f90,$(basename $(PSY)))
PSY_OBJ = $(PSY_SRC:.f90=.o)

# PSyclone command-line options
PSYCLONE_CMD = --psykal-dsl lfric -nodm -l all --config $(PSYCLONE_RELPATH)/config/psyclone.cfg

# Executable
EXEC = time_evolution

# Settings of the PSyData-specific variables are done by the
# Makefiles that include this file.
PSYDATA_LIB=$(PSYDATA_PATH)/lib$(PSYDATA_LIB_NAME).a
F90FLAGS += -I $(PSYDATA_PATH)

# LFRic infrastructure library
PSYCLONE_RELPATH = ../../../../..
LFRIC_PATH ?= $(PSYCLONE_RELPATH)/external/lfric_infrastructure/src
LFRIC_NAME = lfric
LFRIC_LIB = $(LFRIC_PATH)/lib$(LFRIC_NAME).a

# This will add the required include flags to F90FLAGS
include $(LFRIC_PATH)/lfric_include_flags.mk

# GungHo auxiliary libraries
GHLIB_PATH = ../gungho_lib
GHLIB_NAME = gungho
GHLIB_LIB = $(GHLIB_PATH)/lib$(GHLIB_NAME).a
F90FLAGS += -I$(GHLIB_PATH) $(LFRIC_INCLUDE_FLAGS)


# Targets
.PHONY: transform compile run clean allclean

$(EXEC): $(LFRIC_LIB) $(GHLIB_LIB) $(PSYDATA_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(PSY_OBJ) -o $(EXEC)        \
	-L$(PSYDATA_PATH) -l$(PSYDATA_LIB_NAME)                \
	-L$(GHLIB_PATH)   -l$(GHLIB_NAME)                      \
	-L$(LFRIC_PATH)   -l$(LFRIC_NAME) $$(nf-config --flibs)

$(GHLIB_LIB): $(LFRIC_LIB)
	$(MAKE) -C $(GHLIB_PATH)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

$(PSYDATA_LIB): $(LFRIC_LIB)
ifneq ($(PSYDATA_PATH),)
	$(MAKE) -C $(PSYDATA_PATH)
endif

# Dependencies
$(OBJ): $(LFRIC_LIB) $(GHLIB) $(PSYDATA_LIB)
time_evolution_driver.o:  time_evolution_alg_mod_psy.o time_evolution_alg_mod.o

time_evolution_alg_mod.o: init_perturbation_kernel_mod.o \
	prop_perturbation_kernel_mod.o time_evolution_alg_mod_psy.o

time_evolution_alg_mod_psy.o: init_perturbation_kernel_mod.o \
	prop_perturbation_kernel_mod.o

%.o: %.mod

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.f90 %_psy.f90: %.x90
	psyclone $(PSYCLONE_CMD) \
	-opsy $*_psy.f90 -oalg $*.f90 $<

clean:
	rm -f *.o *.mod *.txt $(EXEC)  time_evolution_alg_mod.f90 $(PSY_SRC)

allclean: clean
	$(MAKE) -C $(GHLIB_PATH)   clean
	$(MAKE) -C $(LFRIC_PATH)   allclean

# If PSYDATA_PATH is not defined (i.e. this is invoked from
# Makefile, not solution/Makefile.*), we don't know which
# PSyData library to clean ...
ifneq ($(PSYDATA_PATH),)
	$(MAKE) -C $(PSYDATA_PATH) clean
endif
