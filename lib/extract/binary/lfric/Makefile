#------------------------------------------------------------------------------
# BSD 2-Clause License
# 
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#------------------------------------------------------------------------------
# Author: J. Henrichs, Australian Bureau of Meteorology
# Modified: I. Kavcic, Met Office
#           A. R. Porter, STFC Daresbury Lab

# This Makefile picks up the compiler to use plus any flags from
# environment variables. e.g.:
# 
# export F90=mpif90
# export F90FLAGS="-O3"
# export AR=ar
#
# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags

F90FLAGS ?=
ifeq ($(MPI), yes)
	F90 ?= mpif90
else
	F90 ?= gfortran
	F90FLAGS += -DNO_MPI
endif

# Path to the LFRic infrastructure library. It defaults to the pared-down
# version of the LFRic infrastructure distributed with PSyclone.
# Overwrite for a different infrastructure version.
LFRIC_PATH ?= ./../../../../src/psyclone/tests/test_files/lfric/infrastructure
# Paths to the PSyclone wrapper libraries and required templates. They default
# to the relative paths to the top-level 'lib' and 'lib/extract/binary'
# directories. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./../../..
LIB_TMPLT_DIR ?= ./..
# -----------------------------------------------------------------------------

INF_INC = $(LFRIC_PATH)
INF_LIB_NAME = lfric
INF_LIB = $(LFRIC_PATH)/lib$(INF_LIB_NAME).a

# This will setup the required include flags in $(LFRIC_INCLUDE_FLAGS)
include $(LFRIC_PATH)/lfric_include_flags.inc

PSYDATA_LIB_NAME = _extract
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a

PROCESS_ARGS = -prefix=extract_ -types=char,int,logical,real,double \
                       -dims=1,2,3,4
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

OBJS = psy_data_base.o kernel_data_binary.o extract_binary_base.o \
	   compare_variables_mod.o

default: $(PSYDATA_LIB)

.PHONY: default clean allclean

# We add read_kernel_data_mod as a convenience for using this
# in LFRic: all required file for kernel extractions and drivers
# are then in this directory. That file does not need to be compiled,
# it will be included in the created driver programs.
$(PSYDATA_LIB): $(INF_LIB) $(OBJS) read_kernel_data_mod.f90
	${AR} ${ARFLAGS} ${PSYDATA_LIB} $(OBJS)

# Create LFRic infrastructure library
$(INF_LIB):
	$(MAKE) -C $(LFRIC_PATH)

kernel_data_binary.o:	extract_binary_base.o
extract_binary_base.o:	psy_data_base.o

kernel_data_binary.f90: kernel_data_binary.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) $< > kernel_data_binary.f90

psy_data_base.f90: $(PSYDATA_LIB_DIR)/psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare $< > psy_data_base.f90

read_kernel_data_mod.f90: $(LIB_TMPLT_DIR)/read_kernel_data_mod.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) $< > read_kernel_data_mod.f90

extract_binary_base.F90: $(LIB_TMPLT_DIR)/extract_binary_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-provide $< > extract_binary_base.F90

compare_variables_mod.F90: $(PSYDATA_LIB_DIR)/extract/compare_variables_mod.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare -generic-provide $< > compare_variables_mod.F90

%.o: %.f90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<

clean:
	rm -f *.o *.mod $(PSYDATA_LIB) psy_data_base.f90 extract_binary_base.F90

allclean: clean
	$(MAKE) -C $(LFRIC_PATH) allclean
