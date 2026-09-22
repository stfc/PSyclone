# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# This Makefile just creates the Fortran base class for the compare_variables_mod
# from the jinja template `compare_variables_mod.jinja`. The compile functionality
# it offers is only meant for testing, each domain=specific library should compile
# its own version (with therefore consistent preprocessing and compiler settings).

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?=
# Path to the PSyclone wrapper libraries. It defaults to the relative path to
# the top-level 'lib' directory. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./..

# These arguments are responsible for creating the Fortran
# `compare_variables_mod` file based on the jinja template. It will support
# the listed data types and scalar and 2-dimensional arrays.
# The type and n-arity of the comparable type may need to be adjusted to
# match the capabilities of the target application using the arguments below.

PROCESS_ARGS ?= -types=char,int,long,logical,real,double -dims=1,2,3,4
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

.PHONY: all compare test
	
compare: compare_variables_mod.o

test_compare: compare_variables_mod.o test_compare.o
	$(F90) $(F90FLAGS) $^ -o $@

make test: test_compare
	# The first 6 fields of the output contain the counts of various
	# errors. Only check these counts, not the computed floating point
	# differences (which are the next 4 columns), since they might
	# slightly vary between compilers and hardware platforms.
	./test_compare | grep "a_dbl    *15 *1 *2 *3 *4 *5" > /dev/null
	./test_compare | grep "a_single *10 *1 *0 *2 *3 *4" > /dev/null
	./test_compare | grep "a_int    *15 *1 *2 *3 *4 *5" > /dev/null

%.o: %.F90
	$(F90) -c $(F90FLAGS) $<

%.o: %.f90
	$(F90) -c $(F90FLAGS) $<

# ------------------------------------------------
compare_variables_mod.F90:      compare_variables_mod.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) $< > compare_variables_mod.F90

# Handy shortcut to trigger compilation of all libraries
all: compare
	$(MAKE) -C ascii all
	$(MAKE) -C binary all
	$(MAKE) -C netcdf all

allclean: clean
	$(MAKE) -C ascii allclean
	$(MAKE) -C binary allclean
	$(MAKE) -C netcdf allclean

clean:
	rm -f compare_variables_mod.mod compare_variables_mod.o \
		  compare_variables_mod.F90 test_compare.mod test_compare.o \
		  test_compare
	$(MAKE) -C ascii clean
	$(MAKE) -C binary clean
	$(MAKE) -C netcdf clean
