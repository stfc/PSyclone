# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology

include ../../common.mk

GENERATED_FILES = mixed_precision_psy.f90 alg.f90 ${EXEC} ${OBJ} *.mod

F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound


OBJ = alg.o mixed_precision_psy.o mixed_kernel_mod.o

EXEC = will_not_run.exe
LFRIC_PATH = ../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_PATH)/lib$(LFRIC_NAME).a

# This will add the required include flags to LFRIC_INCLUDE_FLAGS
include $(LFRIC_PATH)/lfric_include_flags.inc

.PHONY: transform compile run

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) -L$(LFRIC_PATH) -l$(LFRIC_NAME)

compile: transform $(EXEC)

transform:
	${PSYCLONE} algorithm.x90 -opsy mixed_precision_psy.f90 -oalg alg.f90

alg.f90 mixed_precision_psy.f90: transform
alg.o: mixed_precision_psy.o
mixed_precision_psy.o: mixed_kernel_mod.o

run: compile
	@echo "No run targets for lfric/eg19"

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)


%.o: %.F90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<

allclean: clean
	make -C $(LFRIC_PATH) allclean
