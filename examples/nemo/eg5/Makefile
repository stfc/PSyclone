# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# ------------------------------------------------------------------------------
# Author: A. R. Porter, STFC Daresbury Lab
# Modified J. Henrichs, Bureau of Meteorology

# Makefile for the 5th NEMO example. Uses PSyclone (which must
# be installed) to generate Fortran for the tracer-advection benchmark.
# The script provided here instruments each of the kernels
# with code to extract input- and output-data into a data file.

# The compiler to use may be specified via the F90 environment variable,
# it defaults to:
# export F90=gfortran
# export F90FLAGS="-g -O0"


include ../../common.mk

# There are two different extraction libraries that can be used:
# 1) standalone, which uses Fortan binary data files to store the data
# 2) netcdf, which uses NetCDF for writing the data files.
# The library can be selected by setting TYPE to either 'standalone'
# (which is the default) or 'netcdf'. Note that both libraries implement
# the same API, so no change to any script is required for switching
# between the two libraries, but recompilation is required (since the
# .mod files will be different).
TYPE ?= standalone

GENERATED_FILES += psy.f90 psy.o traadv-$(TYPE).exe output.dat

# Pick up the right extraction library:
PSYROOT=../../..
ifeq ($(TYPE), netcdf)
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/netcdf/nemo
	READ_DIR ?= $(PSYROOT)/lib/extract/netcdf
	F90FLAGS += $$(nf-config --fflags)
	LDFLAGS += $$(nf-config --flibs)
	GENERATED_FILES += tra_adv-r?.nc
else
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/standalone/nemo
	READ_DIR ?= $(PSYROOT)/lib/extract/standalone
	GENERATED_FILES += tra_adv-r?.binary
endif

F90FLAGS += -I$(EXTRACT_DIR) -I$(READ_DIR)
LIB_NAME = lib_extract.a


.PHONY: allclean

compile: traadv-$(TYPE).exe

run: compile
	IT=2 JPI=10 JPJ=10 JPK=5  ./traadv-$(TYPE).exe

traadv-$(TYPE).exe: psy.o $(EXTRACT_DIR)/$(LIB_NAME)
	$(F90) psy.o -o traadv-$(TYPE).exe $(EXTRACT_DIR)/$(LIB_NAME) $(LDFLAGS)

transform: kernels

# Need `-l all` to ensure line-lengths in generated code are less than the
# standard-mandated 132 chars.
kernels: extract_kernels.py
	$(PSYCLONE) -l all -api "nemo" -s ./extract_kernels.py -opsy psy.f90 \
   ../code/tra_adv.F90

$(EXTRACT_DIR)/$(LIB_NAME):
	make -C $(EXTRACT_DIR)

# Compilation uses the 'kernels' transformed code
psy.f90: kernels
psy.o:  $(EXTRACT_DIR)/$(LIB_NAME)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<
