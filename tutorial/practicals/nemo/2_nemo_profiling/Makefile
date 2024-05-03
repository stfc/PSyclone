# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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

include ../../../common.mk

.PHONY: clean allclean transform compile run

# We use the PSyData simple_timing implementation since it has no
# dependencies.
PROFILE_DIR ?= ../../../../lib/profiling/simple_timing
PROFILE_LINK = -lsimple_timing
PROFILE_LIB = ${PROFILE_DIR}/libsimple_timing.a

NAME = ./tra_adv.exe

# Use gfortran by default
F90 ?= gfortran

$(NAME): $(KERNELS) psy.o runner.o
	$(F90) $^ -o $@ -L$(PROFILE_DIR) $(PROFILE_LINK)

psy.f90: tra_adv_mod.F90
	$(PSYCLONE) --profile routines -api nemo \
                     -opsy psy.f90 -l output tra_adv_mod.F90
#	$(PSYCLONE) -s ./profile_trans.py -api nemo \
#                     -opsy psy.f90 -l output tra_adv_mod.F90

# Make sure the infrastructure library is compiled, so the mods are found
$(KERNELS): $(INF_LIB)

# Creating psy.o and runner.o requires that the profiling lib be compiled
psy.o: ${PROFILE_LIB}
runner.o: psy.o ${PROFILE_LIB}

%.o: %.f90
	$(F90) $(F90FLAGS) -I $(PROFILE_DIR) -c $< -o $@

${PROFILE_LIB}:
	${MAKE} -C ${PROFILE_DIR}

clean:
	rm -f psy.f90 psy_*.f90 *.o *.mod $(NAME) output.dat solutions/*.o

allclean: clean
	${MAKE} -C ${PROFILE_DIR} allclean

transform:
	# Use different output filenames so this doesn't interfere
	# with building the binary (which uses psy.f90)
	$(PSYCLONE) --profile invokes -api nemo \
                     -opsy psy_1.f90 -l output tra_adv_mod.F90
	$(PSYCLONE) -s ./profile_trans.py -api nemo \
                     -opsy psy_2.f90 -l output tra_adv_mod.F90
	$(PSYCLONE) -s ./solutions/named_profile_trans.py -api nemo \
                     -opsy psy_3.f90 -l output tra_adv_mod.F90

compile: transform $(KERNELS) psy.o solutions/runner.o
	$(F90) $(KERNELS) psy.o solutions/runner.o -o $(NAME) \
	       -L$(PROFILE_DIR) $(PROFILE_LINK)

# Only used for the compile CI target to compile the solution file
solutions/%.o: solutions/%.f90
	$(F90) $(F90FLAGS) -I $(PROFILE_DIR) -c $< -o $@


run: compile
	JPK=30 JPJ=100 JPI=100 IT=10 $(NAME)
