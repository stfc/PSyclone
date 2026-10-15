# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# This Makefile picks up the compiler to use plus any flags from
# environment variables. e.g.:
# 
# export F90=mpif90
# export F90FLAGS="-O3"
# export AR=ar

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?=
# Path to the LFRic infrastructure library. It defaults to the included
# version of the LFRic infrastructure distributed with PSyclone.
# Overwrite for a different infrastructure version.
LFRIC_PATH ?= ./../../../external/lfric_infrastructure/src
# Paths to the PSyclone wrapper libraries and required templates. They default
# to the relative paths to the top-level 'lib' and 'lib/read_only'
# directories. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./../..
LIB_TMPLT_DIR ?= ./..
# -----------------------------------------------------------------------------

INF_LIB_NAME = lfric
INF_LIB = $(LFRIC_PATH)/lib$(INF_LIB_NAME).a

include $(LFRIC_PATH)/lfric_include_flags.mk

F90FLAGS += $(LFRIC_INCLUDE_FLAGS)

PSYDATA_LIB_NAME = _read_only
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a

# The LFRic ReadOnly library needs the PreDeclareVariable() and
# ProvideVariable() routines for the basic Fortran types real, int,
# and double, and for 1-, 2-, 3-, and 4-dimensional arrays. These arguments
# are used for both base classes: PSyDataBase and ReadOnlyBase to ensure
# consistent code creation.
PROCESS_ARGS = -prefix=read_only_verify_ -types=int,logical,real,double \
		       -dims=1,2,3,4
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

OBJS = read_only.o psy_data_base.o read_only_base.o

default: $(PSYDATA_LIB)

.PHONY: default clean allclean

$(PSYDATA_LIB): $(INF_LIB) $(OBJS)
	${AR} ${ARFLAGS} ${PSYDATA_LIB} $^

# Add all dependencies. The LFRic ReadOnly library extends the
# ReadOnlyBase type, which in turn extends the PSyDataBase type.
# --------------------------------------------------------------
read_only.o: 	  $(INF_LIB) read_only.f90 read_only_base.o

read_only_base.o: psy_data_base.o

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

read_only.f90: read_only.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) $< > read_only.f90

# The LFRic ReadOnly library is built on the psy-data base library
# and the read-only base library. This library uses the `declareXXX`
# functions implemented in `psy_data_base`, hence the parameter
# `-generic-declare`. It also uses the `provideXXX` functions
# implemented in `read_only_base`, which are added with the
# `-generic-provide` parameter.
psy_data_base.f90: $(PSYDATA_LIB_DIR)/psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

read_only_base.f90: $(LIB_TMPLT_DIR)/read_only_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > read_only_base.f90

# Create LFRic infrastructure library
$(INF_LIB):
	$(MAKE) -C $(LFRIC_PATH)

clean:
	rm -f *.o *.mod $(PSYDATA_LIB) psy_data_base.* read_only.f90 read_only_base.*

allclean: clean
	$(MAKE) -C $(LFRIC_PATH) allclean
