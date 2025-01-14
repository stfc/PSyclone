# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
