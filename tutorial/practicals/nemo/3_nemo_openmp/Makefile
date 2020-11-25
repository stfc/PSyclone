# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

# Makefile for the PSyclone NEMO OpenMP tutorial.

# We use the PSyData simple_timing implementation since it has no
# dependencies.
PROFILE_DIR ?= ../../../../lib/profiling/simple_timing
PROFILE_LINK = -lsimple_timing
PROFILE_LIB = ${PROFILE_DIR}/libsimple_timing.a

NAME = tra_adv.exe
# The PSyclone transformation script to use
TRANS_SCRIPT ?= ./omp_trans.py

# Use gfortran by default
F90 ?= gfortran
F90FLAGS ?= -fopenmp -O2

PSYCLONE = psyclone -api nemo -opsy psy.f90 -l output

# The solutions scripts that the 'test' target will execute
SOLUTION_SCRIPTS = $(wildcard solutions/*.py)

.PHONY: test clean allclean ${SOLUTION_SCRIPTS}

$(NAME): $(KERNELS) psy.o runner.o
	$(F90) $(F90FLAGS) $^ -o $@ -L$(PROFILE_DIR) $(PROFILE_LINK)

# Target that uses PSyclone to generate 'psy.f90'
psy.f90: tra_adv_mod.F90 ${TRANS_SCRIPT}
	${PSYCLONE} -s ${TRANS_SCRIPT} tra_adv_mod.F90

# Make sure the infrastructure library is compiled, so the mods are found
$(KERNELS): $(INF_LIB)

# Creating psy.o and runner.o requires that the profiling lib be compiled
runner.o psy.o: ${PROFILE_LIB}

%.o: %.f90
	$(F90) $(F90FLAGS) -I $(PROFILE_DIR) -c $<

${PROFILE_LIB}:
	${MAKE} -C ${PROFILE_DIR}

clean:
	rm -f psy.f90 *.o *.mod

allclean: clean
	rm -f ${NAME}
	${MAKE} -C ${PROFILE_DIR}  clean

test: ${SOLUTION_SCRIPTS}

${SOLUTION_SCRIPTS}:
	${PSYCLONE} -s $@ tra_adv_mod.F90
	${PSYCLONE} -s $@ --profile invokes tra_adv_mod.F90
	${PSYCLONE} -s $@ --profile kernels tra_adv_mod.F90
