# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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
# Modified: A. R. Porter, STFC Daresbury Laboratory

# This Makefile picks up the compiler to use plus any flags from
# environment variables. e.g.:
#     export F90=mpif90
#     export F90FLAGS="-O3"
# The dl_esm_inf infrastructure library:
#     export INF_DIR=../../../external/dl_esm_inf/finite_difference/src
# If you are using the dl_timer, you can also use:
#     export DL_TIMER_ROOT=../../../../apeg-dl_timer
# Similarly for DrHook:
#     export DRHOOK_DIR=../../../../drhook

include ../../common.mk

GENERATED_FILES += *.o *.mod profile_test.* alg.f90 psy.f90

# Location of the infrastucture code (which is a submodule of the
# PSyclone git repo).
SHARED_DIR ?= ../../../external
INF_DIR ?= $(SHARED_DIR)/dl_esm_inf/finite_difference
INF_INC = $(INF_DIR)/src
INF_LIB = $(INF_DIR)/src/lib_fd.a

# The two kernels used in the application.
KERNELS = init_field_mod.o update_field_mod.o

# The name of the executable
NAME=profile_test
.PHONY: compile transform all template simple_timing drhook dl_timer lfric
.PHONY: template_lib simple_timing_lib drhook_lib dl_timer_lib lfric_lib

run: compile
	./profile_test.template

compile: transform template
transform: psy.f90
all: template simple_timing drhook dl_timer lfric

template:
	$(MAKE) WRAPPER_DIR="../../../lib/profiling/template" \
	        WRAPPER_NAME="dummy"                          \
	        template_lib $(NAME).template

simple_timing: 
	$(MAKE) WRAPPER_DIR="../../../lib/profiling/simple_timing" \
	        WRAPPER_NAME="simple_timing"                       \
	        simple_timing_lib $(NAME).simple_timing

# Needs to additionally link with DrHook library. As default
# assume DrHook is installed 'next' to PSyclone
DRHOOK_DIR?=../../../../drhook
drhook:
	$(MAKE) WRAPPER_DIR="../../../lib/profiling/drhook"        \
	        WRAPPER_NAME="drhook_psy"                          \
			PROFILE_LINK="-L $(DRHOOK_DIR)/build/lib -ldrhook" \
	        drhook_lib $(NAME).drhook

# As default assume apeg-dl_timer is installed 'next' to PSyclone.
DL_TIMER_ROOT?=../../../../apeg-dl_timer
# Needs to link with openmp because apeg-dl_timer library needs openmp. 
dl_timer:
	$(MAKE) WRAPPER_DIR="../../../lib/profiling/dl_timer"    \
	        WRAPPER_NAME="dl_timer_psy"                      \
			PROFILE_LINK="-L$(DL_TIMER_ROOT) -ldl_timer_omp -fopenmp" \
	        dl_timer_lib $(NAME).dl_timer

# The LFRic timer can be used for gocean as well - the wrapper provides
# a standalone library that contains all dependencies and can therefore be
# used with any program.
lfric: 
	$(MAKE) WRAPPER_DIR="../../../lib/profiling/lfric"  \
	        WRAPPER_NAME="psy_lfric_timer_standalone"                    \
	        lfric_lib $(NAME).lfric

# Generic rule. It temporarily compiles a profile-wrapper-specific version
# of psy (psy_template.o, psy_drhook.o, ...), which 'make' then deletes
# automatically as an intermediate file. You can uncomment the following line
# in order to preserve temporary files:
#.SECONDARY:

$(NAME).%: $(INF_LIB) $(WRAPPER_DIR)/lib$(WRAPPER_NAME).a $(KERNELS) psy.o alg.o
	$(F90) $(F90FLAGS) $(KERNELS) psy.o alg.o -o $@                             \
		   -L $(WRAPPER_DIR) -l $(WRAPPER_NAME) \
		   $(PROFILE_LINK) $(INF_LIB)

alg.f90 psy.f90: test.x90
	${PSYCLONE} --profile invokes -api "gocean1.0" \
		     -opsy psy.f90 -oalg alg.f90 test.x90

# Make sure the infrastructure library is compiled, so the mods are found
$(KERNELS): $(INF_LIB) $(DEP_LIB)

# The dl_esm_inf library
$(INF_LIB):
	$(MAKE) -C $(INF_DIR)

drhook_lib :
	$(MAKE) DRHOOK_ROOT=$(DRHOOK_DIR) -C $(WRAPPER_DIR)

dl_timer_lib:
	$(MAKE) DL_TIMER_ROOT=$(DL_TIMER_ROOT) -C $(WRAPPER_DIR)

lfric_lib:
	$(MAKE) DL_TIMER_ROOT=$(DL_TIMER_ROOT) -C $(WRAPPER_DIR)

template_lib simple_timing_lib:
	$(MAKE) -C $(WRAPPER_DIR)

$(DEP_LIB):
	$(MAKE) -C ../../../lib/profiling/simple_timing

# Special rule to create psy_LIBNAME.o for each wrapper library (required
# since PSyDataType will differ from one wrapper library to the next)
psy.o: psy.f90 $(KERNELS)
	$(F90) -o $@ $(F90FLAGS) -I $(INF_INC) -I $(WRAPPER_DIR) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -I $(INF_INC) -I $(WRAPPER_DIR) -c $<

allclean: clean
	${MAKE} -C ../../../lib/profiling allclean
	$(MAKE) -C $(INF_DIR) allclean
