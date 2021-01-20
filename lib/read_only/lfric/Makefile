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


# This Makefile picks up the compiler to use plus any flags from
# environment variables. e.g.:
# 
# export F90=mpif90
# export F90FLAGS="-O3"
# export AR=ar

#TODO: #813 keeps track of adding enough files from LFRic to PSyclone to be
#      able to compile this library without the need of external
#      files (or #745, which will try to create a stand-alone example).

# This directory points to an external, compiled version of the LFRic,
# which is required to find the module files when compiling this library:
LFRIC_DIR ?= ../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure/
LFRIC_LIB=$(LFRIC_DIR)/liblfric.a

# The LFRic ReadOnly library needs the PreDeclareVariable() and
# ProvideVariable() routines for the basic Fortran types real, int,
# and double, and for 1-, 2-, 3-, and 4-dimensional arrays. These arguments
# are used for both base classes: PSyDataBase and ReadOnlyBase to ensure
# consistent code creation.
PROCESS_ARGS = -prefix=read_only_verify_ -types=real,int,double \
		       -dims=1,2,3,4
PROCESS=$$(../../get_python.sh) ../../process.py

F90 ?= gfortran
F90FLAGS ?=
F90FLAGS += -I$(LFRIC_DIR)
LIB_NAME = lib_read_only.a

OBJS = read_only.o psy_data_base.o read_only_base.o

default: $(LIB_NAME)
.PHONY: default clean


$(LIB_NAME): $(OBJS)
	${AR} ${ARFLAGS} ${LIB_NAME} $^

# Add all dependencies. The LFRic ReadOnly library extends the
# ReadOnlyBase type, which in turn extends the PSyDataBase type.
# --------------------------------------------------------------
read_only.o: 	  $(LFRIC_LIB) read_only.f90 read_only_base.o

read_only_base.o: psy_data_base.o

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# The LFRic ReadOnly library is built on the psy-data base library
# and the read-only base library. This library uses the `declareXXX`
# functions implemented in `psy_data_base`, hence the parameter
# `-generic-declare`. It also uses the `provideXXX` functions
# implemented in `read_only_base`, which are added with the
# `-generic-provide` parameter.
psy_data_base.f90: ../../psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

read_only_base.f90: ../read_only_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > read_only_base.f90

# Create LFRic infrastructure library if required
$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_DIR)

clean:
	rm -f *.o *.mod $(LIB_NAME) psy_data_base.* read_only_base.*
