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
# Author: J. Henrichs, Bureau of Meteorology

include ../common.mk

OBJ = main_alg_psy.o main_alg.o summation_w0_to_w3_kernel_mod.o

EXEC = example

# Various targets
# ---------------

# Default target: run PSyclone (even if the output file should already exist)
test:
	$(PSYCLONE) -s ./omp_transformation.py -nodm -l output \
		     -opsy main_alg_psy.f90 -oalg main_alg.f90 main_alg.x90

compile: $(EXEC)

run: compile
	@# If OMP_NUM_THREADS is not define, set it to 3:
	OMP_NUM_THREADS=$${OMP_NUM_THREADS:-3} ./example

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) -L$(LFRIC_PATH) -l$(LFRIC_NAME) \
		   $$(nf-config --flibs)

clean:
	rm -f $(EXEC) $(OBJ) *.mod main_*alg.f90 main_*psy.f90

allclean: clean
	make -C $(LFRIC_PATH) allclean

# Keep the generated psy and alg files
.precious: main_alg_psy.f90 main_alg.f90

# This rule invokes the same PSyclone command, but it won't run if
# the output file already exist, which is useful to avoid recompilation.
main_alg_psy.f90: main_alg.x90 omp_transformation.py
	$(PSYCLONE) -s ./omp_transformation.py -nodm -l output \
			 -opsy main_alg_psy.f90 -oalg main_alg.f90 main_alg.x90

# Dependencies
# ------------
main_alg.f90: main_alg_psy.f90
main_alg_psy.o:	summation_w0_to_w3_kernel_mod.o
main_alg.o:	main_alg_psy.o
summation_w0_to_w3_kernel_mod.o: $(LFRIC_LIB)
