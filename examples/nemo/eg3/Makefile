# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2021, Science and Technology Facilities Council.
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

# Makefile for the 3rd NEMO example. Uses PSyclone (which must
# be installed) to generate Fortran with OpenACC directives for
# the tracer-advection benchmark. The 'compile' target requires
# the dl_timer library (bitbucket.org/apeg/dl_timer) which must
# be installed and the location specified in DL_TIMER_ROOT below.
#
# The compiler to use must be specified via the F90 environment
# variable. e.g. to use PGI and OpenACC:
#
# export F90=pgf90
# export F90FLAGS="-O1 -acc -ta=tesla,cc70 -Minfo=all"
# export LDFLAGS="-acc -ta=tesla,cc70"
include ../../common.mk

GENERATED_FILES += psy.f90 psy.o traadv.exe psyclone.log traldf_iso.F90

F90FLAGS ?= -fopenmp
LDFLAGS ?= -fopenmp

# Location of the dl_timer library
DL_TIMER_ROOT ?= ${HOME}/Projects/dl_timer
TIMER_INC = ${DL_TIMER_ROOT}/src
TIMER_LIB = ${DL_TIMER_ROOT}/libdl_timer_omp.a

.PHONY: allclean dl_timer

# Compilation requires that dl_timer be installed
compile: traadv.exe

traadv.exe: dl_timer psy.o
	${F90} ${LDFLAGS} psy.o ${TIMER_LIB} -o traadv.exe

dl_timer:
	${MAKE} -C ${DL_TIMER_ROOT} F90="${F90}" F90FLAGS="${F90FLAGS}" \
   LDFLAGS="${LDFLAGS}" sm_lib

transform: kernels managed_mem

# Need `-l all` to ensure line-lengths in generated code are less than the
# standard-mandated 132 chars
kernels:
	${PSYCLONE} -l all -api "nemo" -s ./kernels_trans.py -opsy psy.f90 \
   ../code/tra_adv.F90

# Example of using the more sophisticated script in ../scripts to
# transform for OpenACC assuming the use of managed memory.
# TODO #852 - simplify that script and merge it with the functionality of
# the scripts in this directory.
managed_mem:
	${PSYCLONE} -l all -api "nemo" -s ../scripts/kernels_trans.py \
   ../code/tra_adv.F90
	PSYCLONE="${PSYCLONE}" ../scripts/process_nemo.py \
   -s ../scripts/kernels_trans.py ../code/traldf_iso.F90

# Compilation uses the 'kernels' transformed code
psy.f90: kernels

%.o: %.f90
	$(F90) $(F90FLAGS) -I${TIMER_INC} -c $<

all: transform compile

allclean: clean
	# Only clean DL_TIMER if it actually exists
	if [ -d ${DL_TIMER_ROOT}  ]; then \
		${MAKE} -C ${DL_TIMER_ROOT} allclean; \
	fi
