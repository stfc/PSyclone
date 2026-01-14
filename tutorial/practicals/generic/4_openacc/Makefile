# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2026, Science and Technology Facilities Council.
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
# Modified: J. Henrichs, Bureau of Meteorology
# Modified: S. Siso, STFC Daresbury Lab

# Makefile for the PSyclone NEMO OpenACC tutorial.

include ../../../common.mk

# By default we only generate the transformed code
.DEFAULT_GOAL := output_1.f90

# The PSyclone transformation script to use
TRANS_SCRIPT ?= ./kernels_trans.py

# Use gfortran by default
F90 ?= gfortran
F90FLAGS ?= -fopenacc -O2

# For NVIDIA we could use:
# F90FLAGS = -acc -O2 -Minfo=all -Mnofma
# F90 = nvfortran
# (Note that '-Mnofma' is required to get identical results from CPU and
# GPU for this benchmark.)

# ------------------------------------------------------------------------------
NAME = ./tra_adv.exe

.PHONY: clean allclean transform

$(NAME): $(KERNELS) output_openacc.o runner.o
	$(F90) $(F90FLAGS) $^ -o $@

# Target that uses PSyclone to generate 'output%.f90' - it is used
# to create output_openacc.f90 here, but also to create output_1 as
# default target.
output%.f90: tra_adv_mod.F90 ${TRANS_SCRIPT}
	$(PSYCLONE) -s ${TRANS_SCRIPT} -o $@ -l output tra_adv_mod.F90

.precious: output_openacc.f90

# Make sure the infrastructure library is compiled, so the mods are found
$(KERNELS): $(INF_LIB)

# Dependencies - runner needs tra_adv.mod, which is in the psyclone
# processed file output_openacc.o
runner.o: output_openacc.o

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f output.f90 output_*.f90 *.o *.mod $(NAME) output.dat

allclean: clean

transform:
	${MAKE} -C solutions transform

compile: TRANS_SCRIPT=./solutions/collapse_trans.py
compile: transform $(NAME)

run: compile
	JPK=30 JPJ=100 JPI=100 IT=10 $(NAME)

# Runs the OpenACC version, checks the number of kernel launches and
# checks that the outputs match the KGOs from CPU. (On NVIDIA, the
# latter step requires the '-Mnofma' compiler flag.)
acc_test: compile
	./solutions/test_acc.sh
