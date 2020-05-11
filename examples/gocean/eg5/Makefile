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

# This Makefile picks up the compiler to use plus any flags from
# environment variables. e.g.:
#     export F90=mpif90
#     export F90FLAGS="-O3"
# The dl_esm_inf infrastructure library:
#     export DL_DIR=../../../external/dl_esm_inf/finite_difference/src
# If you are using the dl_timer, you can also use:
#     export DL_TIMER_ROOT=../../../../apeg-dl_timer
# Similarly for DrHook:
#     export DRHOOK_DIR=../../../../drhook

F90 ?= gfortran
F90FLAGS ?=
DL_DIR ?= ../../../external/dl_esm_inf/finite_difference/src

KERNELS = init_field_mod.o update_field_mod.o
NAME=profile_test

.PHONY: default template simple_timing drhook dl_timer

default: template
all: template simple_timing drhook dl_timer

template: PROFILE_DIR=../../../lib/profiling/template
template: PROFILE_LINK=-ldummy
template: $(NAME).template

simple_timing: PROFILE_DIR=../../../lib/profiling/simple_timing
simple_timing: PROFILE_LINK=-lsimple_timing
simple_timing: $(NAME).simple_timing

# Needs to additionally link with drhook library. As default
# assume it is installed 'next' to PSyclone
drhook: DRHOOK_DIR?=../../../../drhook
drhook: PROFILE_DIR=../../../lib/profiling/drhook
drhook: PROFILE_LINK=-ldrhook_psy -L $(DRHOOK_DIR)/build/lib -ldrhook
drhook: $(NAME).drhook

# Needs to compile with openmp and link in apeg-dl_timer library. As
# default assume apeg-dl_timer is installed 'next' to PSyclone.
dl_timer: DL_TIMER_ROOT?=../../../../apeg-dl_timer
dl_timer: PROFILE_DIR=../../../lib/profiling/dl_timer
dl_timer: PROFILE_LINK=-ldl_timer -L$(DL_TIMER_ROOT) -l:dl_timer_lib.a -fopenmp
dl_timer: F90FLAGS += -fopenmp   # apeg-dl_timer must be compiled with openmp!
dl_timer: $(NAME).dl_timer

# Generic rule. It temporarily compiles a profile-wrapper-specific version
# of psy (psy_template.o, psy_drhook.o, ...), which 'make' then deletes
# automatically as an intermediate file. You can uncomment the following line
# in order to preserve temporary files:
#.SECONDARY:

$(NAME).%:	$(KERNELS) psy_%.o alg.o 
	$(F90) $^ -o $@ -L$(PROFILE_DIR) $(PROFILE_LINK) -L $(DL_DIR) -l_fd

alg.f90 psy.f90: test.x90
	psyclone --profile invokes -l -api "gocean1.0" \
			 --config ../../../config/psyclone.cfg \
		     -opsy psy.f90 -oalg alg.f90 test.x90

# Special rule to create psy_LIBNAME.o for each wrapper library (required
# since PSyDataType will differ from one wrapper library to the next)
psy_%.o: psy.f90 $(KERNELS)
	$(F90) -o $@ $(F90FLAGS) -I $(DL_DIR) -I $(PROFILE_DIR) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -I $(DL_DIR) -I $(PROFILE_DIR) -c $<

clean:
	rm -f *.o *.mod alg.f90 psy.f90 profile_test.*
