# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# export F90=nvfortran
# export F90FLAGS="-acc"

PSYROOT=../../..

include $(PSYROOT)/examples/common.mk

GENERATED_FILES = *.o *.mod  $(EXEC) main_alg.f90 main_psy.f90

F90 ?= gfortran
F90FLAGS ?= -Wall -g

OBJ = main_psy.o main_alg.o testkern_w0_kernel_mod.o

EXEC = example_openacc

LFRIC_DIR ?= $(PSYROOT)/src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_DIR)/lib$(LFRIC_NAME).a

F90FLAGS += -I$(LFRIC_DIR)

.PHONY: transformtransform_kernels transform_parallel transform_parallel_dm

transform: transform_kernels transform_parallel transform_parallel_dm

transform_kernels:
	${PSYCLONE} -nodm -s ./acc_kernels.py \
	-opsy main_psy.f90 -oalg main_alg.f90 main.x90

transform_parallel:
	${PSYCLONE} -nodm -s ./acc_parallel.py \
	-opsy main_psy.f90 -oalg main_alg.f90 main.x90

transform_parallel_dm:
	${PSYCLONE} -dm -s ./acc_parallel_dm.py \
	-opsy main_psy.f90 -oalg main_alg.f90 main.x90


%_psy.f90:	%.x90
	${PSYCLONE} -s ./acc_parallel_dm.py \
	-opsy $*_psy.f90 -oalg $*_alg.f90 $<

#TODO #1669 - the code currently does not compile
#             set_dirty calls inside openacc region
#TODO #1694 - the code currently does not compile
#             incorrect variable names and constants
#             when using builtin
compile: transform_parallel_dm $(EXEC)

run: compile
	./$(EXEC)

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) -L$(LFRIC_DIR) -l$(LFRIC_NAME)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_DIR)

# Dependencies
main_psy.o:	testkern_w0_kernel_mod.o
main_alg.o:	main_psy.o

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# Keep the generated psy and alg files
.precious: main_psy.f90 main_alg.f90

main_alg.f90: main_psy.f90

%_psy.f90:	%.x90
	${PSYCLONE} -s ./acc_parallel_dm.py \
	-opsy $*_psy.f90 -oalg $*_alg.f90 $<

allclean: clean
	$(MAKE) -C $(LFRIC_DIR) allclean
