# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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

# This Makefile just creates the Fortran base class for all extraction
# libraries from the jinja template. The compile functionality it offers is
# only meant for testing, each library should create and compile its own
# version of the base class (with therefore consistent compiler and
# jinja settings).

F90 ?= gfortran
F90FLAGS ?= 

F90FLAGS += $$(nf-config --fflags)

# The extract library is implemented for int, real and
# double scalars and 2-dimension arrays
PROCESS_ARGS = -prefix=extract_ -types=int,real,double \
		       -dims=2
PROCESS=$$(../../get_python.sh) ../../process.py

default: extract_netcdf_base.o psy_data_base.o
process: extract_netcdf_base.f90

.PHONY: default process clean all

all:
	$(MAKE) -C dl_esm_inf
	$(MAKE) -C lfric


# The dependencies
# ----------------
extract_netcdf_base.o: psy_data_base.o

# Jinja rules for creating the base class, and the
# netcdf extraction class
# ------------------------------------------------
psy_data_base.f90:	../../psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

extract_netcdf_base.f90:	extract_netcdf_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > extract_netcdf_base.f90

# Compilation
# -----------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f extract_netcdf_base.f90 psy_data_base.f90 *.o *.mod
