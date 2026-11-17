# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Makefile for the 9th NEMO example. Uses PSyclone (which must
# be installed) to generate Fortran for the tracer-advection benchmark.
# The script provided here instruments each outer do loop with the value
# range check transformation, which verifies that at runtime that variables
# are within a user-specified range.

# The compiler to use may be specified via the F90 environment variable,
# it defaults to:
# export F90=gfortran
# export F90FLAGS="-g -O0"

include ../../common.mk

EXEC = traadv.exe
GENERATED_FILES += psy.f90 psy.o $(EXEC) output.dat

# Pick up the right extraction library:
PSYROOT=../../..
VALUE_RANGE_DIR ?= $(PSYROOT)/lib/value_range_check/generic
LIB_NAME = lib_value_range_check.a

F90FLAGS += -I$(VALUE_RANGE_DIR)

.PHONY: allclean

compile: $(EXEC)

run: compile
	# Note that umask is expected to be between 0 and 1, so the two 'error'
	# messages are normal and show that the range check works.
	PSY_VALUE_RANGE="umask=0.0:0.9" IT=2 JPI=10 JPJ=10 JPK=5  ./$(EXEC) 2>&1 | \
		grep "Variable 'umask' has the value"

$(EXEC): psy.o $(VALUE_RANGE_DIR)/$(LIB_NAME)
	$(F90) $(F90FLAGS) psy.o -o $(EXEC) $(VALUE_RANGE_DIR)/$(LIB_NAME) $(LDFLAGS)

transform: kernels

# Need `-l all` to ensure line-lengths in generated code are less than the
# standard-mandated 132 chars.
kernels: value_range_check_transformation.py
	$(PSYCLONE) --keep-comments --keep-directives -l all \
		-s ./value_range_check_transformation.py         \
		-o psy.f90 ../code/tra_adv.F90

$(VALUE_RANGE_DIR)/$(LIB_NAME):
	${MAKE} -C $(VALUE_RANGE_DIR)

# Compilation uses the 'kernels' transformed code
psy.f90: kernels
psy.o:  $(VALUE_RANGE_DIR)/$(LIB_NAME)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

allclean: clean
	${MAKE} -C $(VALUE_RANGE_DIR) allclean
