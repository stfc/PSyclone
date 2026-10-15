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
#
# It also uses nf-config to query for flags required for NetCDF compilation.

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?=
# Path to the 'dl_esm_inf' infrastructure library. It defaults to the version
# distributed with PSyclone. Overwrite for a different infrastructure version.
GOCEAN_INF_DIR ?= ./../../../external/dl_esm_inf/finite_difference
# Paths to the PSyclone wrapper libraries and required templates. They default
# to the relative paths to the top-level 'lib' and 'lib/value_range_check'
# directories. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./../..
LIB_TMPLT_DIR ?= ./..
# -----------------------------------------------------------------------------

INF_INC = $(GOCEAN_INF_DIR)/src
INF_LIB_NAME = _fd
INF_LIB = $(GOCEAN_INF_DIR)/src/lib$(INF_LIB_NAME).a

F90FLAGS += -I$(INF_INC)
PSYDATA_LIB_NAME = _value_range_check
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a

OBJS = value_range_check.o psy_data_base.o value_range_check_base.o

# The arguments for the jinja templates. GOcean only needs
# 2-dimensional arrays, and int, real- and double basic Fortran types
PROCESS_ARGS = -prefix=value_range_check_ -types=int,real,double \
		       -dims=2
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

default: $(PSYDATA_LIB)

.PHONY: default clean allclean

$(PSYDATA_LIB): $(INF_LIB) $(OBJS)
	${AR} ${ARFLAGS} ${PSYDATA_LIB} $^

# Dependencies:
# -------------
# Compilation of value_range_check needs the .mod file from dl_esm_inf,
# the base class value_range_check_base (which in turn needs psy_data_base).
value_range_check.o: $(INF_LIB) value_range_check.f90 value_range_check_base.o

value_range_check_base.o: psy_data_base.o

# Rules for file creation
# -----------------------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# The dl_esm_inf ValueRangeCheck library is built on the PSyData base library
# and the ValueRangeCheck base library. So add the rules to process
# these files to get the plain Fortran files:
psy_data_base.f90: $(PSYDATA_LIB_DIR)/psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

value_range_check_base.f90: $(LIB_TMPLT_DIR)/value_range_check_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > value_range_check_base.f90

# The dl_esm_inf library
$(INF_LIB):
	$(MAKE) -C $(GOCEAN_INF_DIR)

clean:
	rm -f *.o *.mod $(PSYDATA_LIB) psy_data_base.* value_range_check_base.*

allclean: clean
	$(MAKE) -C $(GOCEAN_INF_DIR) clean
