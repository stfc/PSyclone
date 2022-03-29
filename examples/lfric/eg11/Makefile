# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Authors: A. R. Porter, STFC Daresbury Lab
#          J. Henrichs, Bureau of Meteorology
# Modifications: R. W. Ford, STFC Daresbury Lab

# The compiler to use may be specified via the F90 environment variable
#
# export F90=gfortran
# export F90FLAGS="-g -O0"

include ../../common.mk

GENERATED_FILES = helmholtz_solver_alg_psy.f90 helmholtz_solver_alg_mod.f90 *.o *.mod $(EXEC)

F90 ?= gfortran
F90FLAGS ?= -g -O0

OBJ = helmholtz_solver_alg_psy.o helmholtz_solver_alg_mod.o scaled_matrix_vector_kernel_mod.o

EXEC = will_not_run.exe
LFRIC_PATH = ../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_PATH)/lib$(LFRIC_NAME).a

F90FLAGS += -I$(LFRIC_PATH)

# This example will not run as no mesh is set up and the data is not
# initialised in the algorithm layer. The example was simply designed
# to check that the generated code would compile.
run: compile
	@echo "Running not supported for lfric/eg11"

compile: transform $(EXEC)

transform: helmholtz_solver_alg_psy.f90 helmholtz_solver_alg_mod.f90

helmholtz_solver_alg_psy.f90 helmholtz_solver_alg_mod.f90: helmholtz_solver_alg_mod.x90
	${PSYCLONE} -s ./async_script.py -opsy helmholtz_solver_alg_psy.f90 -oalg helmholtz_solver_alg_mod.f90 $<

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) -L$(LFRIC_PATH) -l$(LFRIC_NAME)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

# Dependencies
helmholtz_solver_alg_psy.o:	scaled_matrix_vector_kernel_mod.o
helmholtz_solver_alg_mod.o:	helmholtz_solver_alg_psy.o
scaled_matrix_vector_kernel_mod.o: $(LFRIC_LIB)

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

allclean: clean
	make -C $(LFRIC_PATH) allclean
