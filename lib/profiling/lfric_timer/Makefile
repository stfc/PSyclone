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
# Author J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

# This Makefile compiles a PSyData profiling wrapper library using the LFRic
# timer code. It can be used with any code, not only with LFRic.

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?= -g
# Path to the LFRic infrastructure library. It defaults to the pared-down
# version of the LFRic infrastructure distributed with PSyclone.
# Overwrite for a different infrastructure version.
LFRIC_INF_DIR ?= ./../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure
# -----------------------------------------------------------------------------

INF_INC = $(LFRIC_INF_DIR)
INF_LIB_NAME = lfric
INF_LIB = $(LFRIC_INF_DIR)/lib$(INF_LIB_NAME).a

F90FLAGS += -I$(INF_INC)
PSYDATA_LIB_NAME = psy_lfric_timer
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a
PSYDATA_STANDALONE_LIB_NAME = psy_lfric_timer_standalone
PSYDATA_STANDALONE_LIB = lib$(PSYDATA_STANDALONE_LIB_NAME).a

PSY_OBJS = profile_psy_data_mod.o
INF_OBJS = timer_mod.o log_mod.o scalar_mod.o mpi_mod.o io_utility_mod.o
INF_OBJS_FULL_PATH = $(patsubst %,$(LFRIC_INF_DIR)/%,$(INF_OBJS))

.phony: default clean allclean

default: $(PSYDATA_LIB) $(PSYDATA_STANDALONE_LIB)

# First target: this needs to be linked with the LFRic
# infrastructure library.
$(PSYDATA_LIB): $(INF_LIB) $(PSY_OBJS)
	ar rs $@ $<

# Second target: this library contains the required files
# from the infrastructure library, and can be used by any
# application, not only LFRic.
$(PSYDATA_STANDALONE_LIB): $(INF_LIB) $(PSY_OBJS)
	ar rs $@ $(INF_OBJS_FULL_PATH) $(PSY_OBJS)

# Trigger recompilation if the infrastructure lib is changed
$(OBJS): $(INF_LIB)

# Create infrastructure library
$(INF_LIB):
	$(MAKE) -C $(LFRIC_INF_DIR)

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f $(PSYDATA_LIB) $(PSYDATA_STANDALONE_LIB) *.mod *.o

allclean: clean
	$(MAKE) -C $(LFRIC_INF_DIR) allclean
