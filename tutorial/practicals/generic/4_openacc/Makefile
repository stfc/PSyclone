# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
