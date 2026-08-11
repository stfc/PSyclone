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
# Author: A. R. Porter, STFC Daresbury Laboratory
# Modified J. Henrichs, Bureau of Meteorology

# The compiler to use may be specified via the F90 environment variable
#
# export F90=gfortran
# export F90FLAGS="-g -O0"

include ../lfric_common.mk

GENERATED_FILES = single_invoke_alg.f90 single_invoke_psy.f90 \
	single_invoke_psy.o *.mod testkern_mod.o

F90 ?= gfortran
F90FLAGS ?= -g -O0

.PHONY: transform compile run

# The '-d ../code' argument specifies that PSyclone should search the
# '../code' directory when looking for user-supplied kernels.
transform: single_invoke_alg.f90
	@echo "With distributed-memory support:"
	${PSYCLONE} -api lfric -d ../code ./single_invoke.x90

single_invoke_alg.f90 single_invoke_psy.f90: single_invoke.x90
	@echo "Sequential code:"
	${PSYCLONE} -api lfric -d ../code -nodm ./single_invoke.x90 \
		-oalg single_invoke_alg.f90 \
		-opsy single_invoke_psy.f90

compile: $(LFRIC_LIB) single_invoke_psy.o single_invoke_alg.o

testkern_mod.o: ../code/testkern_mod.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

single_invoke_alg.o: single_invoke_psy.o
single_invoke_psy.o: testkern_mod.o
testkern_mod.o: $(LFRIC_LIB)

run: compile
	@echo "No run targets for lfric/eg1"
