# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2026, Science and Technology Facilities Council.
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
