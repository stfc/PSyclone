# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2025, Science and Technology Facilities Council.
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
