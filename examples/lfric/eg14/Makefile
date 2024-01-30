# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# ------------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology
# Modifications: A. R. Porter, STFC Daresbury Laboratory

# The compiler to use may be specified via the F90 and F90FLAGS
# environment variables. To use the NVIDIA compiler and enable
# openacc compilation, use:
#
# export F90=nvfortran
# export F90FLAGS="-acc=gpu -Minfo=all"

PSYROOT=../../..

include $(PSYROOT)/examples/common.mk

GENERATED_FILES = *.o *.mod $(EXEC) main_alg.f90 main_psy.f90 \
                  other_alg_mod_psy.f90 other_alg_mod_alg.f90 \
                  testkern_w0_kernel_?_mod.f90

F90 ?= gfortran
F90FLAGS ?= -Wall -g

OBJ = main_psy.o main_alg.o other_alg_mod_psy.o other_alg_mod_alg.o \
      testkern_w0_kernel_0_mod.o

EXEC = example_openacc

LFRIC_PATH ?= $(PSYROOT)/src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_PATH)/lib$(LFRIC_NAME).a
# This sets up LFRIC_INCLUDE_FLAGS
include $(LFRIC_PATH)/lfric_include_flags.inc

# PSyData profiling wrapper library.
# Default is to use the 'template' library which simply prints entry and exit
# messages to stdout.
PROFILE_PATH=$(PSYROOT)/lib/profiling/template
PROFILE_LIB=$(PROFILE_PATH)/libdummy.a
PROFILE_LINK=-ldummy
# For NVIDIA profiling
#PROFILE_PATH=$(PSYROOT)/lib/profiling/nvidia
#PROFILE_LIB=$(PROFILE_PATH)/libnvtx_prof.a
#PROFILE_LINK="-lnvtx_prof -lnvToolsExt"

.PHONY: transform compile run

# This makefile assumes that the transformed kernel will be named
# 'testkern_w0_kernel_0_mod.f90'. However, if it already exists then PSyclone
# will create 'testkern_..._1_mod.f90' so remove it first.
transform:
	rm -f testkern_w0_kernel_0_mod.f90
	${MAKE} main_psy.f90
	${MAKE} other_alg_mod_psy.f90

# Instruct PSyclone to automatically put profiling calipers around every
# invoke.
%_psy.f90: %.x90
	${PSYCLONE} -dm -s ./acc_parallel.py  --profile invokes \
	-opsy $*_psy.f90 -oalg $*_alg.f90 $<

testkern_w0_kernel_0_mod.f90: main_psy.f90

compile: transform ${EXEC}

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

run: compile
	./$(EXEC)

$(EXEC): $(PROFILE_LIB) $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) $(OBJ) -o $(EXEC) -L$(LFRIC_PATH) -l$(LFRIC_NAME) -L${PROFILE_PATH} $(PROFILE_LINK)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

$(PROFILE_LIB):
	$(MAKE) -C $(PROFILE_PATH)

# Dependencies
main_psy.o:	other_alg_mod_psy.o testkern_w0_kernel_0_mod.o
main_alg.o:	other_alg_mod_alg.o main_psy.o testkern_w0_kernel_0_mod.o

%.o: %.F90
	$(F90) $(F90FLAGS) -I$(PROFILE_PATH) $(LFRIC_INCLUDE_FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -I$(PROFILE_PATH) $(LFRIC_INCLUDE_FLAGS) -c $<

# Keep the generated psy and alg files
.precious: main_psy.f90 main_alg.f90

main_alg.f90: main_psy.f90
other_alg_mod_alg.f90: other_alg_mod_psy.f90

allclean: clean
	$(MAKE) -C $(LFRIC_PATH) allclean
	$(MAKE) -C $(PROFILE_PATH) allclean
