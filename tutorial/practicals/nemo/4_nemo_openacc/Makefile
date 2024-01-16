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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified J. Henrichs, Bureau of Meteorology

# Makefile for the PSyclone NEMO OpenACC tutorial.

include ../../../common.mk

# By default we only generate the transformed code
.DEFAULT_GOAL := psy_1.f90

# The PSyclone transformation script to use
TRANS_SCRIPT ?= ./kernels_trans.py

# Use gfortran by default
F90 ?= gfortran
F90FLAGS ?= -fopenacc -O2

# For NVIDIA we could use:
# F90FLAGS = -acc -ta=tesla -O2 -Minfo=all
# F90 = nvfortran

# ------------------------------------------------------------------------------
NAME = ./tra_adv.exe

.PHONY: clean allclean transform

$(NAME): $(KERNELS) psy_openacc.o runner.o
	$(F90) $(F90FLAGS) $^ -o $@

# Target that uses PSyclone to generate 'psy%.f90' - it is used
# to create psy_openacc.f90 here, but also to create psy_1 as
# default target.
psy%.f90: tra_adv_mod.F90 ${TRANS_SCRIPT}
	$(PSYCLONE) -s ${TRANS_SCRIPT} -api nemo \
                     -opsy $@ -l output tra_adv_mod.F90

.precious: psy_openacc.f90

# Make sure the infrastructure library is compiled, so the mods are found
$(KERNELS): $(INF_LIB)

# Dependencies - runner needs tra_adv.mod, which is in the psyclone
# processed file psy_openacc.c.
runner.o: psy_openacc.o

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f psy.f90 psy_*.f90 *.o *.mod $(NAME) output.dat

allclean: clean

transform:
	${MAKE} -C solutions transform

compile: TRANS_SCRIPT=./solutions/collapse_trans.py
compile: transform $(NAME)

run: compile
	JPK=30 JPJ=100 JPI=100 IT=10 $(NAME)
