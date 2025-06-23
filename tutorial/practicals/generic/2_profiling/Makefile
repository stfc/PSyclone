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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: J. Henrichs, Bureau of Meteorology
# Modified: S. Siso, STFC Daresbury Lab

include ../../../common.mk

.PHONY: clean allclean transform compile run

# By default we use the PSyData simple_timing implementation since it has no
# dependencies.
PROFILE_WRAPPER_DIR ?= ../../../../lib/profiling/simple_timing
PROFILE_WRAPPER_LIB ?= ${PROFILE_WRAPPER_DIR}/libsimple_timing.a
PROFILE_WRAPPER_LINK ?= ${PROFILE_WRAPPER_LIB}
# There's no separate library for 'simple_timing' - its functionality
# is incorporated in the PSyData wrapper.
PROFILE_DIR ?=
PROFILE_LIB ?=
PROFILE_LINK ?=

# For dl_timer:
#
# NOTE: dl_timer *requires* an initialisation call to be added
#       to runner.f90:
#         use profile_psy_data_mod, only : profile_PSyDataInit
#         call profile_PSyDataInit()
#
# (git clone https://bitbucket.org/apeg/dl_timer.git
#  cd dl_timer; . compiler_setup/gnu_setup.sh; make sm_lib)
#PROFILE_WRAPPER_DIR ?= ../../../../lib/profiling/dl_timer
#PROFILE_WRAPPER_LIB ?= ${PROFILE_WRAPPER_DIR}/libdl_timer_psy.a
#PROFILE_WRAPPER_LINK ?= ${PROFILE_WRAPPER_LIB}
#PROFILE_DIR ?= ../../../../../dl_timer
#PROFILE_LIB ?= ${PROFILE_DIR}/libdl_timer_omp.a
# Since we've built the shared-memory version of dl_timer
# ('sm_lib' above) we need to ensure the executable is linked with OpenMP.
#PROFILE_LINK ?= -fopenmp ${PROFILE_LIB}

NAME = ./tra_adv.exe

# Use gfortran by default
F90 ?= gfortran

$(NAME): $(KERNELS) output.o runner.o
	$(F90) $^ -o $@ $(PROFILE_WRAPPER_LINK) $(PROFILE_LINK)

output.f90: tra_adv_mod.F90
	$(PSYCLONE) --profile routines -o output.f90 -l output tra_adv_mod.F90

# Make sure the infrastructure library is compiled, so the mods are found
$(KERNELS): $(INF_LIB)

# Creating output.o and runner.o requires that the profiling-wrapper lib be
# compiled
output.o: ${PROFILE_WRAPPER_LIB}
runner.o: output.o ${PROFILE_WRAPPER_LIB}

%.o: %.f90
	$(F90) $(F90FLAGS) -I $(PROFILE_WRAPPER_DIR) -c $< -o $@

${PROFILE_WRAPPER_LIB}: ${PROFILE_LIB}
	${MAKE} -C ${PROFILE_WRAPPER_DIR}

clean:
	rm -f output.f90 output_*.f90 *.o *.mod $(NAME) output.dat solutions/*.o

allclean: clean
	${MAKE} -C ${PROFILE_WRAPPER_DIR} allclean

transform:
	# Use different output filenames so this doesn't interfere
	# with building the binary (which uses output.f90)
	$(PSYCLONE) --profile routines -o output_1.f90 -l output tra_adv_mod.F90
	$(PSYCLONE) -s ./profile_trans.py -o output_2.f90 -l output tra_adv_mod.F90
	$(PSYCLONE) -s ./solutions/named_profile_trans.py \
                     -o output_3.f90 -l output tra_adv_mod.F90

compile: transform $(KERNELS) output.o solutions/runner.o
	$(F90) $(F90FLAGS) $(KERNELS) output.o solutions/runner.o -o $(NAME) \
	       $(PROFILE_WRAPPER_LINK) $(PROFILE_LINK)

# Only used for the compile CI target to compile the solution file
solutions/%.o: solutions/%.f90
	$(F90) $(F90FLAGS) -I $(PROFILE_WRAPPER_DIR) -c $< -o $@


run: compile
	JPK=30 JPJ=100 JPI=100 IT=10 $(NAME)
