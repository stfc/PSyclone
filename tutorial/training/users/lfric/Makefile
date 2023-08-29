# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2023, Science and Technology Facilities Council.
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

# The compiler to use may be specified via the F90 environment
#
# export F90=gfortran
# export F90FLAGS="-Wall -g -fcheck=bound"
F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

OBJ = main_psy.o main_alg.o testkern_w3_kernel_mod.o

EXEC = example
# Path of the included simplified LFRic infrastructure files:
LFRIC_PATH = ../../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_PATH)/lib$(LFRIC_NAME).a

# This sets up LFRIC_INCLUDE_FLAGS
include $(LFRIC_PATH)/lfric_include_flags.inc
F90FLAGS += $(LFRIC_INCLUDE_FLAGS)

# Various targets
# ---------------
.PHONY: transform compile run clean allclean

transform: main_psy.f90

omp:
	psyclone -s omp.py -nodm -l output -opsy main_omp_psy.f90 -oalg main_omp_alg.f90 main.x90

compile: transform $(EXEC)

run: compile
	./example

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) -L$(LFRIC_PATH) -l$(LFRIC_NAME)

clean:
	rm -f $(EXEC) $(OBJ) *.mod main_*alg.f90 main_*psy.f90

allclean: clean
	make -C $(LFRIC_PATH) allclean

# External libraries - LFRic infrastructure
# -----------------------------------------
$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

# Dependencies
# ------------
main_alg.f90: main_psy.f90
main_psy.o:	testkern_w3_kernel_mod.o
main_alg.o:	main_psy.o
testkern_w3_kernel_mod.o: $(LFRIC_LIB)

# File creation rules
# -------------------
%.o: %.f90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<

# Keep the generated psy and alg files
.precious: main_psy.f90 main_alg.f90

%_psy.f90:	%.x90
	psyclone -nodm -l output -opsy $*_psy.f90 -oalg $*_alg.f90 $<
