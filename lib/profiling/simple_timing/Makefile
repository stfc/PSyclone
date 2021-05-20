# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
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

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?=
# Path to the PSyclone wrapper libraries. It defaults to the relative path to
# the top-level 'lib' directory. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./../..
# -----------------------------------------------------------------------------

PSYDATA_LIB_NAME = simple_timing
PSYDATA_LIB := lib$(PSYDATA_LIB_NAME).a

OBJS = $(PSYDATA_LIB_NAME).o psy_data_base.o

PROCESS_ARGS = -prefix=profile_ -types= -dims=
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

.PHONY: default clean allclean

default: $(PSYDATA_LIB)

$(PSYDATA_LIB): $(OBJS)
	ar rs $@ $(OBJS)

$(PSYDATA_LIB_NAME).o:	psy_data_base.o

psy_data_base.f90: $(PSYDATA_LIB_DIR)/psy_data_base.jinja
	$(PROCESS) $(PROCESS_ARGS) $< > psy_data_base.f90

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f *.o *.mod $(PSYDATA_LIB) psy_data_base.*

allclean: clean
