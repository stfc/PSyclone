# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# This Makefile just creates the Fortran base class for all extraction
# libraries from the jinja template. The compile functionality it offers is
# only meant for testing, each library should create and compile its own
# version of the base class (with therefore consistent compiler and
# jinja settings).

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90FLAGS ?=
ifeq ($(MPI), yes)
       F90 ?= mpif90
else
       F90 ?= gfortran
       F90FLAGS += -DNO_MPI
endif

# Path to the PSyclone wrapper libraries. It defaults to the relative path to
# the top-level 'lib' directory. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./../..
# -----------------------------------------------------------------------------

# The extract library is implemented for character, int, long, logical, real,
# and double scalars and 1- to 4-dimensional arrays
PROCESS_ARGS = -prefix=extract_ -types=char,int,long,logical,real,double \
		       -dims=1,2,3,4
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

default: extract_ascii_base.o psy_data_base.o read_kernel_data_mod.o

process: extract_ascii_base.F90

.PHONY: default process all clean allclean

all:
	$(MAKE) -C dl_esm_inf
	$(MAKE) -C lfric
	$(MAKE) -C generic

# The dependencies
# ----------------
extract_ascii_base.o: psy_data_base.o

# Jinja rules for creating the base class, and the
# ascii extraction class
# ------------------------------------------------
psy_data_base.f90:	$(PSYDATA_LIB_DIR)/psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

extract_ascii_base.F90:	extract_ascii_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > extract_ascii_base.F90

read_kernel_data_mod.f90:	read_kernel_data_mod.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > read_kernel_data_mod.f90

# Compilation
# -----------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f extract_ascii_base.F90 psy_data_base.f90 read_kernel_data_mod.f90
	rm -f *.o *.mod

allclean:
	$(MAKE) -C dl_esm_inf allclean
	$(MAKE) -C lfric allclean
	$(MAKE) -C generic allclean
