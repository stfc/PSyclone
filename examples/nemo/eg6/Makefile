# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# This example uses PSyclone (which must be installed) to generate Fortran for
# the tracer-advection benchmark. The script provided here instruments code to
# check that read-only variables are not modified.

# The compiler to use may be specified via the F90 environment variable,
# it defaults to:
# export F90=gfortran
# export F90FLAGS="-g -O0"

include ../../common.mk

TYPE ?= standalone

GENERATED_FILES += psy.f90 psy.o dummy

PSYROOT=../../..
READ_ONLY_CHECK_DIR ?= $(PSYROOT)/lib/read_only/generic

F90FLAGS += -I$(READ_ONLY_CHECK_DIR)
LIB_NAME = lib_read_only.a

.PHONY: allclean

compile: dummy

run: compile
	./dummy

dummy: psy.o $(READ_ONLY_CHECK_DIR)/$(LIB_NAME)
	$(F90) $(F90FLAGS) psy.o -o dummy $(READ_ONLY_CHECK_DIR)/$(LIB_NAME) $(LDFLAGS)

transform: kernels

# Need `-l all` to ensure line-lengths in generated code are less than the
# standard-mandated 132 chars.
kernels: read_only_check.py
	$(PSYCLONE) -l all -s ./read_only_check.py -o psy.f90 dummy.f90

$(READ_ONLY_CHECK_DIR)/$(LIB_NAME):
	$(MAKE) -C $(READ_ONLY_CHECK_DIR)

# Compilation uses the 'kernels' transformed code
psy.f90: dummy.f90 read_only_check.py kernels
psy.o:  $(READ_ONLY_CHECK_DIR)/$(LIB_NAME)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

allclean: clean
	${MAKE} -C $(READ_ONLY_CHECK_DIR) allclean
