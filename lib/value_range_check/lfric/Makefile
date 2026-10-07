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
# Path to the LFRic infrastructure library. It defaults to the
# version of the LFRic infrastructure distributed with PSyclone.
# Overwrite for a different infrastructure version.
LFRIC_PATH ?= ../../../external/lfric_infrastructure/src

# Paths to the PSyclone wrapper libraries and required templates. They default
# to the relative paths to the top-level 'lib' and 'lib/value_range_check'
# directories. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./../..
LIB_TMPLT_DIR ?= ./..
# -----------------------------------------------------------------------------

INF_LIB_NAME = lfric
INF_LIB = $(LFRIC_PATH)/lib$(INF_LIB_NAME).a

include $(LFRIC_PATH)/lfric_include_flags.mk

F90FLAGS += $(LFRIC_INCLUDE_FLAGS)
PSYDATA_LIB_NAME = _value_range_check
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a

# The LFRic NaN checking library needs the PreDeclareVariable() and
# ProvideVariable() routines for the basic Fortran types real, int,
# and double, and for 1-, 2-, 3-, and 4-dimensional arrays. These arguments
# are used for both base classes: PSyDataBase and NANTestBase to ensure
# consistent code creation.
PROCESS_ARGS = -prefix=value_range_check_ -types=real,int,logical,double \
		       -dims=1,2,3,4
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

OBJS = value_range_check.o psy_data_base.o value_range_check_base.o

default: $(PSYDATA_LIB)

.PHONY: default clean allclean

$(PSYDATA_LIB): $(INF_LIB) $(OBJS)
	${AR} ${ARFLAGS} ${PSYDATA_LIB} $^

# Create LFRic infrastructure library
$(INF_LIB):
	$(MAKE) -C $(LFRIC_PATH) -f ../Makefile

# Add all dependencies. The LFRic NaN checking library extends the
# NANTestBase type, which in turn extends the PSyDataBase type.
# ---------------------------------------------------------------
value_range_check.o: 	  $(INF_LIB) value_range_check.f90 value_range_check_base.o

value_range_check_base.o: psy_data_base.o

# Rules for file creation:
# ------------------------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

value_range_check.f90: value_range_check.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) $< > value_range_check.f90

# The LFRic NaN checking library is built on the PSyData base library
# and the NAN-checking base library. So add the rules to process
# these files to get the plain Fortran files:
psy_data_base.f90: $(PSYDATA_LIB_DIR)/psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

value_range_check_base.f90: $(LIB_TMPLT_DIR)/value_range_check_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > value_range_check_base.f90

clean:
	rm -f *.o *.mod $(PSYDATA_LIB) psy_data_base.* value_range_check_base.*

allclean: clean
	$(MAKE) -C $(LFRIC_PATH) allclean
