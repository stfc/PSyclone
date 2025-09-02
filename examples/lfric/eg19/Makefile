# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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

include ../lfric_common.mk

GENERATED_FILES = mixed_precision_psy.f90 alg.f90 ${EXEC} ${OBJ} *.mod

F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

OBJ = alg.o mixed_precision_psy.o mixed_kernel_mod.o

EXEC = will_not_run.exe

.PHONY: transform compile run

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) $(LDFLAGS)

compile: transform $(EXEC)

# Runs PSyclone to do the code generation. Also demonstrates the use of the
# example 'kernel_print' transformation which prints the Fortran of each
# kernel found.
transform:
	${PSYCLONE} -api lfric algorithm.x90 -s ../scripts/kernel_print.py -opsy mixed_precision_psy.f90 -oalg alg.f90

alg.f90 mixed_precision_psy.f90: transform
alg.o: mixed_precision_psy.o
mixed_precision_psy.o: mixed_kernel_mod.o

run: compile
	@echo "No run targets for lfric/eg19"

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

allclean: clean
	make -C $(LFRIC_PATH) allclean
