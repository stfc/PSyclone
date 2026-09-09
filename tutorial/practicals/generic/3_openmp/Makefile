# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../../common.mk

# We use the PSyData simple_timing implementation since it has no
# dependencies.
PROFILE_DIR ?= ../../../../lib/profiling/simple_timing
PROFILE_LINK = -lsimple_timing
PROFILE_LIB = ${PROFILE_DIR}/libsimple_timing.a

NAME = ./tra_adv.exe
# The PSyclone transformation script to use
TRANS_SCRIPT ?= ./omp_trans.py

# Use gfortran by default
F90 ?= gfortran
F90FLAGS ?= -fopenmp -O2

MY_PSYCLONE = $(PSYCLONE) -l output

# The solutions scripts that the 'transform' target will execute
SOLUTION_SCRIPTS = $(wildcard solutions/*.py)

.PHONY: ${SOLUTION_SCRIPTS}

$(NAME): $(KERNELS) output.o runner.o
	$(F90) $(F90FLAGS) $^ -o $@ -L$(PROFILE_DIR) $(PROFILE_LINK)

# Target that uses PSyclone to generate 'output.f90'
output.f90: tra_adv_mod.F90 ${TRANS_SCRIPT}
	${MY_PSYCLONE} -s ${TRANS_SCRIPT} -o output.f90 tra_adv_mod.F90

# Make sure the infrastructure library is compiled, so the mods are found
$(KERNELS): $(INF_LIB)

# Creating output.o and runner.o requires that the profiling lib be compiled
runner.o output.o: ${PROFILE_LIB} tra_adv_mod.o

%.o: %.f90
	$(F90) $(F90FLAGS) -I $(PROFILE_DIR) -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -I $(PROFILE_DIR) -c $<

${PROFILE_LIB}:
	${MAKE} -C ${PROFILE_DIR}

clean:
	rm -f output.f90 output_*.f90 *.o *.mod $(NAME) output.dat

allclean: clean
	${MAKE} -C ${PROFILE_DIR}  clean

transform: ${SOLUTION_SCRIPTS}

# We need to invoke make transform first so that the
# files from the solution directory are used.
compile: TRANS_SCRIPT=solutions/omp_trans.py
compile: transform $(NAME)

run: compile
	JPK=30 JPJ=100 JPI=100 IT=10 ./$(NAME)

${SOLUTION_SCRIPTS}:
	# We need to use different output names to support testing in parallel
	# and not interfering with the compile task (which uses output.f90)
	${MY_PSYCLONE} -s $@ -o output_1.f90 tra_adv_mod.F90
	${MY_PSYCLONE} -s $@ -o output_2.f90 --profile routines tra_adv_mod.F90
