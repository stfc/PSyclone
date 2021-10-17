# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Modified: I. Kavcic, Met Office


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
# Path to the LFRic infrastructure library. It defaults to the pared-down
# version of the LFRic infrastructure distributed with PSyclone.
# Overwrite for a different infrastructure version.
LFRIC_INF_DIR ?= ./../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure
# Paths to the PSyclone wrapper libraries and required templates. They default
# to the relative paths to the top-level 'lib' and 'lib/nan_test'
# directories. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./../..
LIB_TMPLT_DIR ?= ./..
# -----------------------------------------------------------------------------

INF_INC = $(LFRIC_INF_DIR)
INF_LIB_NAME = lfric
INF_LIB = $(LFRIC_INF_DIR)/lib$(INF_LIB_NAME).a

F90FLAGS += -I$(INF_INC)
PSYDATA_LIB_NAME = _nan_test
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a

# The LFRic NaN checking library needs the PreDeclareVariable() and
# ProvideVariable() routines for the basic Fortran types real, int,
# and double, and for 1-, 2-, 3-, and 4-dimensional arrays. These arguments
# are used for both base classes: PSyDataBase and NANTestBase to ensure
# consistent code creation.
PROCESS_ARGS = -prefix=nan_test_ -types=real,int,double \
		       -dims=1,2,3,4
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

OBJS = nan_test.o psy_data_base.o nan_test_base.o

default: $(PSYDATA_LIB)

.PHONY: default clean allclean

$(PSYDATA_LIB): $(INF_LIB) $(OBJS)
	${AR} ${ARFLAGS} ${PSYDATA_LIB} $^

# Create LFRic infrastructure library
$(INF_LIB):
	$(MAKE) -C $(LFRIC_INF_DIR)

# Add all dependencies. The LFRic NaN checking library extends the
# NANTestBase type, which in turn extends the PSyDataBase type.
# ---------------------------------------------------------------
nan_test.o: 	  $(INF_LIB) nan_test.f90 nan_test_base.o

nan_test_base.o: psy_data_base.o

# Rules for file creation:
# ------------------------
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# The LFRic NaN checking library is built on the PSyData base library
# and the NAN-checking base library. So add the rules to process
# these files to get the plain Fortran files:
psy_data_base.f90: $(PSYDATA_LIB_DIR)/psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

nan_test_base.f90: $(LIB_TMPLT_DIR)/nan_test_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > nan_test_base.f90

clean:
	rm -f *.o *.mod $(PSYDATA_LIB) psy_data_base.* nan_test_base.*

allclean: clean
	$(MAKE) -C $(LFRIC_INF_DIR) allclean
