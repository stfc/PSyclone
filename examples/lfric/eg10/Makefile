# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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

include ../../common.mk

EXEC = will_not_run.exe

GENERATED_FILES = $(OBJ) *.mod intergrid_3levels_psy.f90 intergrid_3levels.f90 $(EXEC)

# This defaults to the infrastructure library included in PSyclone
LFRIC_PATH ?= ${PSYCLONE_DIR}/external/lfric_infrastructure/preprocessed
LFRIC_NAME=lfric_netcdf
LFRIC_LIB=$(LFRIC_PATH)/lib$(LFRIC_NAME).a

OBJ = intergrid_3levels_psy.o intergrid_3levels.o restrict_kernel_mod.o prolong_kernel_mod.o

# This will add the required include flags to LFRIC_INCLUDE_FLAGS
include $(LFRIC_PATH)/lfric_include_flags.mk

.PHONY: transform compile run kernels

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) -L$(LFRIC_PATH) -l$(LFRIC_NAME) \
	$$(nf-config --flibs)

transform intergrid_3levels_psy.f90 intergrid_3levels.f90:
	${PSYCLONE} -api lfric intergrid_3levels.x90 \
		-opsy intergrid_3levels_psy.f90 -oalg intergrid_3levels.f90

compile: transform $(EXEC)

run: compile
	@echo "No run targets for lfric/eg10"

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

intergrid_3levels_psy.o: prolong_kernel_mod.o restrict_kernel_mod.o
intergrid_3levels.o: intergrid_3levels_psy.o

kernels: prolong_kernel_mod.o restrict_kernel_mod.o

%.o: %.F90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<
