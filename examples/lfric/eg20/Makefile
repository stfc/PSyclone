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
# Modifications: A. R. Porter, STFC Daresbury Laboratory,
#                I. Kavcic, Met Office

# The compiler to use may be specified via the F90 environment
#
# export F90=gfortran
# export F90FLAGS="-Wall -g -fcheck=bound"

include ../../common.mk

GENERATED_FILES = test_alg_mod_psy.f90 testkern_mod.F90 *.o *.mod $(EXEC) \
                  test_alg_mod.[fx]90

F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

OBJ = driver.o test_alg_mod_psy.o test_alg_mod.o testkern_mod.o

EXEC = example
LFRIC_PATH = ../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_PATH)/lib$(LFRIC_NAME).a

F90FLAGS += -I$(LFRIC_PATH)

run: compile
	./example

compile: transform $(EXEC)

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) -L$(LFRIC_PATH) -l$(LFRIC_NAME)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

# Dependencies
driver.o: test_alg_mod.o
test_alg_mod.o: test_alg_mod_psy.o
test_alg_mod_psy.o: test_alg_mod_psy.f90 testkern_mod.o
testkern_mod.o: $(LFRIC_LIB)

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.F90:
	ln -s ../code/$@ .

# Keep the generated psy and alg files
.precious: test_alg_mod_psy.f90 test_alg_mod.f90

# Use the psyclone-kern tool to create an algorithm layer that will
# 'invoke' our kernel
test_alg_mod.x90: testkern_mod.F90
	${KERNEL_ALG_GEN} -api dynamo0.3 -o test_alg_mod.x90 ../code/testkern_mod.F90

transform: test_alg_mod_psy.f90

# Process the generated algorithm layer with PSyclone to generate a compilable
# algorithm file and a suitable PSy-layer subroutine.
test_alg_mod_psy.f90:	test_alg_mod.x90
	${PSYCLONE} -d ../code -opsy test_alg_mod_psy.f90 -oalg test_alg_mod.f90 $<

allclean: clean
	make -C $(LFRIC_PATH) allclean
