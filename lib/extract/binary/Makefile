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
# -----------------------------------------------------------------------------
# Author: J. Henrichs, Australian Bureau of Meteorology
# Modified: I. Kavcic, Met Office

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

# The extract library is implemented for int, real and
# double scalars and 1- to 4-dimensional arrays
PROCESS_ARGS = -prefix=extract_ -types=char,int,long,logical,real,double \
		       -dims=1,2,3,4
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

default: extract_binary_base.o psy_data_base.o read_kernel_data_mod.o

process: extract_binary_base.F90

.PHONY: default process all clean allclean

all:
	$(MAKE) -C dl_esm_inf
	$(MAKE) -C lfric
	$(MAKE) -C generic

# The dependencies
# ----------------
extract_binary_base.o: psy_data_base.o

# Jinja rules for creating the base class, and the
# binary extraction class
# ------------------------------------------------
psy_data_base.f90:	$(PSYDATA_LIB_DIR)/psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

extract_binary_base.F90:	extract_binary_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > extract_binary_base.F90

read_kernel_data_mod.f90:	read_kernel_data_mod.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > read_kernel_data_mod.f90

# Compilation
# -----------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f extract_binary_base.F90 psy_data_base.f90
	rm -f *.o *.mod

allclean:
	$(MAKE) -C dl_esm_inf allclean
	$(MAKE) -C lfric allclean
	$(MAKE) -C generic allclean
