# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Makefile for the 3rd NEMO example. Uses PSyclone (which must
# be installed) to generate Fortran with OpenACC directives for
# the tracer-advection benchmark. The 'compile' target requires
# the dl_timer library (github.com/stfc/dl_timer) which must
# be installed and the location specified in DL_TIMER_ROOT below.
#
# The compiler to use must be specified via the F90 environment
# variable. e.g. to use the NVIDIA compiler and OpenACC:
#
# export F90=nvfortran
# export F90FLAGS="-O1 -acc -gpu=cc70 -Minfo=all"
# export LDFLAGS="-acc -gpu=cc70"
include ../../common.mk

GENERATED_FILES += psy.f90 psy.o traadv.exe output.dat psyclone.log \
  traldf_iso.F90

F90FLAGS ?= -fopenacc
LDFLAGS ?= ${F90FLAGS}

.PHONY: allclean

compile: traadv.exe

run: compile
	 IT=2 JPI=10 JPJ=10 JPK=5  ./traadv.exe

traadv.exe: psy.o
	${F90} ${LDFLAGS} psy.o -o traadv.exe

transform: kernels managed_mem

# Need `-l all` to ensure line-lengths in generated code are less than the
# standard-mandated 132 chars
kernels:
	${PSYCLONE} -l all -s ./kernels_trans.py -o psy.f90 ../code/tra_adv.F90

# Example of using the more sophisticated script in ../scripts to
# transform for OpenACC assuming the use of managed memory.
managed_mem:
	${PSYCLONE} -l all -s ../scripts/acc_kernels_trans.py ../code/tra_adv.F90
	${PSYCLONE} -s ../scripts/acc_kernels_trans.py ../code/traldf_iso.F90

# Compilation uses the 'kernels' transformed code
psy.f90: kernels

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<
