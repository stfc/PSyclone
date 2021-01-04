# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Australian Bureau of Meteorology
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


# This defaults to the infrastructure library included in PSyclone
LFRIC_DIR ?= ../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_LIB=$(LFRIC_DIR)/liblfric.a

F90?=gfortran
F90FLAGS?=-g
F90FLAGS += -I$(LFRIC_DIR)

OBJS = profile_psy_data_mod.o

name=profile_psy_data_mod
libname=lib$(name).a

.phony: default clean

default: $(libname)

$(libname): $(LFRIC_LIB) $(OBJS)
	ar rs $(libname) $(name).o

# Trigger recompilation is the infrastructure lib is changed
$(OBJS): $(LFRIC_LIB)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_DIR)

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f $(libname) *.mod *.o

