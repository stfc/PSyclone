# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# This Makefile just creates the Fortran base class for the PSyData library
# from the jinja template psy_data_base.jinja. The compile functionality it
# offers is only meant for testing, each library should compile its own version
# of the base class (with therefore consistent compiler settings).

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?=
# Relative path of wrapper libraries in PSyclone (overwrite for custom location)
PSYDATA_LIB_DIR ?= .
# -----------------------------------------------------------------------------

default: psy_data_base.o

process: psy_data_base.f90

.PHONY: default process clean all

PROCESS = $$(./get_python.sh) ./process.py

all:
	$(MAKE) -C extract all
	$(MAKE) -C profiling all
	$(MAKE) -C read_only all
	$(MAKE) -C value_range_check all

allclean: clean
	$(MAKE) -C extract allclean
	$(MAKE) -C profiling allclean
	$(MAKE) -C read_only allclean
	$(MAKE) -C value_range_check allclean

%.f90:	%.jinja process.py
	$$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py $< > $*.f90

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<


clean:
	rm -f psy_data_base.f90 *.o *.mod
	$(MAKE) -C extract clean
	$(MAKE) -C profiling clean
	$(MAKE) -C read_only clean
	$(MAKE) -C value_range_check clean
