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
# to the relative paths to the top-level 'lib' and 'lib/read_only'
# directories. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./../..
LIB_TMPLT_DIR ?= ./..
# -----------------------------------------------------------------------------

INF_INC = $(GOCEAN_INF_DIR)/src
INF_LIB_NAME = _fd
INF_LIB = $(GOCEAN_INF_DIR)/src/lib$(INF_LIB_NAME).a

F90FLAGS += -I$(INF_INC)
PSYDATA_LIB_NAME = _read_only
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a

OBJS = read_only.o psy_data_base.o read_only_base.o

# The arguments for the jinja templates. GOcean only needs
# 2-dimensional arrays, and int, real- and double basic Fortran types
PROCESS_ARGS = -prefix=read_only_verify_ -types=int,real,double \
		       -dims=2
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

default: $(PSYDATA_LIB)

.PHONY: default clean allclean

$(PSYDATA_LIB): $(INF_LIB) $(OBJS)
	${AR} ${ARFLAGS} ${PSYDATA_LIB} $^

# Dependencies:
# -------------
# Compilation of read_only needs the .mod file from dl_esm_inf,
# the base class read_only_base (which in turn needs psy_data_base).
read_only.o: $(INF_LIB) read_only.f90 read_only_base.o

read_only_base.o: psy_data_base.o

# Rules for file creation
# -----------------------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# The dl_esm_inf ReadOnly library is built on the psy-data base library
# and the read-only base library. This library uses the `declareXXX`
# functions implemented in `psy_data_base`, hence the parameter
# `-generic-declare`. It also uses the `provideXXX` functions implemented
# in `read_only_base`, which are added with the `-generic-provide` parameter.
psy_data_base.f90: $(PSYDATA_LIB_DIR)/psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

read_only_base.f90: $(LIB_TMPLT_DIR)/read_only_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > read_only_base.f90

# The dl_esm_inf library
$(INF_LIB):
	$(MAKE) -C $(GOCEAN_INF_DIR)

clean:
	rm -f *.o *.mod $(PSYDATA_LIB) psy_data_base.* read_only_base.*

allclean: clean
	$(MAKE) -C $(GOCEAN_INF_DIR) clean
