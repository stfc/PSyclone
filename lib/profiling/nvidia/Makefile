# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Makefile for the PSyclone wrapper to the NVTX profiling library.

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?= -g
# -----------------------------------------------------------------------------

PSYDATA_LIB_NAME = nvtx_prof
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a

default: $(PSYDATA_LIB)

.PHONY: default clean allclean

$(PSYDATA_LIB): $(PSYDATA_LIB_NAME).f90
	$(F90) -c $(F90FLAGS) $<
	ar rs $(PSYDATA_LIB) $(PSYDATA_LIB_NAME).o

clean:
	rm -f *.o *.mod $(PSYDATA_LIB)

allclean: clean
