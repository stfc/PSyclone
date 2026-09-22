# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# This Makefile just creates the Fortran base class for all nan-test
# wrappers from the jinja template. The compile functionality it offers is
# only meant for testing, each library should compile its own version
# of the base class (with therefore consistent compiler settings).

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?=
# Path to the PSyclone wrapper libraries. It defaults to the relative path to
# the top-level 'lib' directory. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./..
# -----------------------------------------------------------------------------

# The value-range check library is implemented for int, logical, characters,
# real, and double precision scalars and 1 to 4 dimensional arrays
PROCESS_ARGS = -prefix=value_range_check_ -types=char,int,logical,real,double \
		       -dims=1,2,3,4
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py


default: value_range_check_base.o psy_data_base.o

process: value_range_check_base.f90

.PHONY: default process clean all

all:
	$(MAKE) -C dl_esm_inf
	$(MAKE) -C generic
	$(MAKE) -C lfric

%.f90:	%.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > $*.f90

psy_data_base.f90:	$(PSYDATA_LIB_DIR)/psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

value_range_check_base.o: psy_data_base.o

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f value_range_check_base.f90 psy_data_base.f90 *.o *.mod
	$(MAKE) -C dl_esm_inf clean
	$(MAKE) -C generic clean
	$(MAKE) -C lfric clean

allclean: clean
	$(MAKE) -C dl_esm_inf allclean
	$(MAKE) -C generic allclean
	$(MAKE) -C lfric allclean
