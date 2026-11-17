# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Makefile for the 5th NEMO example. Uses PSyclone (which must
# be installed) to generate Fortran for the tracer-advection benchmark.
# The script provided here instruments each of the kernels
# with code to extract input- and output-data into a data file.

# The compiler to use may be specified via the F90 environment variable,
# it defaults to:
# export F90=gfortran
# export F90FLAGS="-g -O0"


include ../../common.mk

# There are three different extraction libraries that can be used:
# 1) binary, which uses Fortran binary data files to store the data
#    (which might not be transferable between different machines or
#     or different compilers)
# 2) ascii, which uses Fortran ASCII data files to store the data
#    (which might depending on compiler not have bitwise identical results)
# 3) netcdf, which uses NetCDF for writing the data files.
# The library can be selected by setting TYPE to either 'binary'
# (which is the default), 'ascii', or 'netcdf'. Note that both
# libraries implement the same API, so no change to any script is required
# for switching between the two libraries, but recompilation is required
# (since the .mod files will be different).
TYPE ?= binary

GENERATED_FILES += psy.f90 psy.o traadv-$(TYPE).exe output.dat

# Pick up the right extraction library:
PSYROOT=../../..
ifeq ($(TYPE), netcdf)
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/netcdf/generic
	READ_DIR ?= $(PSYROOT)/lib/extract/netcdf
	F90FLAGS += $$(nf-config --fflags)
	LDFLAGS += $$(nf-config --flibs)
	GENERATED_FILES += tra_adv-tra_adv-r*.nc
else ifeq ($(TYPE), ascii)
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/ascii/generic
	READ_DIR ?= $(PSYROOT)/lib/extract/ascii
	GENERATED_FILES += tra_adv-tra_adv-r*.ascii
else
	EXTRACT_DIR ?= $(PSYROOT)/lib/extract/binary/generic
	READ_DIR ?= $(PSYROOT)/lib/extract/binary
	GENERATED_FILES += tra_adv-tra_adv-r*.binary
endif

F90FLAGS += -I$(EXTRACT_DIR) -I$(READ_DIR)
LIB_NAME = lib_extract.a


.PHONY: allclean

compile: traadv-$(TYPE).exe

run: compile
	IT=2 JPI=10 JPJ=10 JPK=5  ./traadv-$(TYPE).exe

traadv-$(TYPE).exe: psy.o $(EXTRACT_DIR)/$(LIB_NAME)
	$(F90) $(F90FLAGS) psy.o -o traadv-$(TYPE).exe $(EXTRACT_DIR)/$(LIB_NAME) $(LDFLAGS)

transform: kernels

# Need `-l all` to ensure line-lengths in generated code are less than the
# standard-mandated 132 chars.
kernels: extract_kernels.py
	$(PSYCLONE) --keep-comments --keep-directives -l all -s ./extract_kernels.py -o psy.f90 ../code/tra_adv.F90

$(EXTRACT_DIR)/$(LIB_NAME):
	${MAKE} -C $(EXTRACT_DIR)

# Compilation uses the 'kernels' transformed code
psy.f90: kernels
psy.o:  $(EXTRACT_DIR)/$(LIB_NAME)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

allclean: clean
	${MAKE} -C ${EXTRACT_DIR} allclean
