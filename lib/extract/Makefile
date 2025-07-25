# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
#------------------------------------------------------------------------------
# Author: J. Henrichs, Australian Bureau of Meteorology
# Modified: I. Kavcic, Met Office

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
	$(MAKE) -C netcdf all
	$(MAKE) -C binary all
	$(MAKE) -C ascii all

clean:
	rm -f compare_variables_mod.mod compare_variables_mod.o \
		  compare_variables_mod.F90 test_compare.mod test_compare.o \
		  test_compare
