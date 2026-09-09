# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# This Makefile is for the read-only checking lib for generic Fortran code.

# This Makefile picks up the compiler to use plus any flags from
# environment variables. e.g.:
# 
# export F90=mpif90
# export F90FLAGS="-O3"
# export AR=ar
#

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?=

# Paths to the PSyclone wrapper libraries and required templates. They default
# to the relative paths to the top-level 'lib' and 'lib/read_only'
# directories. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./../..
LIB_TMPLT_DIR ?= ./..
# -----------------------------------------------------------------------------

PSYDATA_LIB_NAME = _read_only
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a

OBJS = read_only.o psy_data_base.o read_only_base.o

# The arguments for the jinja templates. Create support for all
# types, and 1-4 dimensional arrays.
PROCESS_ARGS = -prefix=read_only_verify_ -types=int,long,char,logical,real,double \
		       -dims=1,2,3,4
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

default: $(PSYDATA_LIB)

.PHONY: default clean allclean

$(PSYDATA_LIB): $(OBJS)
	${AR} ${ARFLAGS} ${PSYDATA_LIB} $^

# Dependencies:
# -------------
# Compilation of read_only needs the base class read_only_base
# (which in turn needs psy_data_base).
read_only.o: read_only.f90 read_only_base.o

read_only_base.o: psy_data_base.o

# Rules for file creation
# -----------------------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# The generic ReadOnly library is built on the psy-data base library
# and the read-only base library. This library uses the `declareXXX`
# functions implemented in `psy_data_base`, hence the parameter
# `-generic-declare`. It also uses the `provideXXX` functions implemented
# in `read_only_base`, which are added with the `-generic-provide` parameter.
psy_data_base.f90: $(PSYDATA_LIB_DIR)/psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

read_only_base.f90: $(LIB_TMPLT_DIR)/read_only_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > read_only_base.f90

clean:
	rm -f *.o *.mod $(PSYDATA_LIB) psy_data_base.* read_only_base.*

allclean: clean
