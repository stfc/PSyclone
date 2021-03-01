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

# This variable points to the cut-down version of the LFRic
# infrastructure distributed with PSyclone. It can be overwritten
# by the user in order to use a different version.
LFRIC_DIR ?= ../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure

# The LFRic NAN checking library needs the PreDeclareVariable() and
# ProvideVariable() routines for the basic Fortran types real, int,
# and double, and for 1-, 2-, 3-, and 4-dimensional arrays. These arguments
# are used for both base classes: PSyDataBase and NANTestBase to ensure
# consistent code creation.
PROCESS_ARGS = -prefix=nan_test_ -types=real,int,double \
		       -dims=1,2,3,4
PROCESS=$$(../../get_python.sh) ../../process.py

F90 ?= gfortran
F90FLAGS ?=
F90FLAGS += -I$(LFRIC_DIR)
LIB_NAME = lib_nan_test.a

OBJS = nan_test.o psy_data_base.o nan_test_base.o

default: $(LIB_NAME)
.PHONY: default clean


$(LIB_NAME): $(LFRIC_DIR)/liblfric.a $(OBJS)
	${AR} ${ARFLAGS} ${LIB_NAME} $^

$(LFRIC_DIR)/liblfric.a:
	$(MAKE) F90="$(F90)" F90FLAGS="$(F90FLAGS)" -C $(LFRIC_DIR)


# Add all dependencies. The LFRic NAN checkin library extends the
# NanTestBase type, which in turn extends the PSyDataBase type.
# ---------------------------------------------------------------
nan_test.o: 	  $(LFRIC_DIR)/field_mod.mod nan_test.f90 \
			 	  nan_test_base.o

nan_test_base.o: psy_data_base.o


# Rules for file creation:
# ------------------------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# The LFRic NAN checking library is built on the psy-data base library
# and the NAN-checking base library. So add the rules to process
# these files to get the plain Fortran files:
psy_data_base.f90: ../../psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

nan_test_base.f90: ../nan_test_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > nan_test_base.f90

clean:
	rm -f *.o *.mod $(LIB_NAME) psy_data_base.* nan_test_base.*
