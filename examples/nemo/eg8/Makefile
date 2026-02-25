# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Author: T. H. Gibson, Advanced Micro Devices, Inc.

# To run with AMD GPU offloading and ROCTx profiling, use the following
# settings before invoking make:
#   export F90=amdflang
#   export F90FLAGS="-O3 -fopenmp --offload-arch=<arch>"
#   export LDFLAGS="-fopenmp --offload-arch=<arch> -L${ROCM_PATH}/lib -lrocprofiler-sdk-roctx"
#   export PSYCLONE_PROFILING_DIR=${PSYCLONE_DIR}/lib/profiling/amd
#   export PSYCLONE_PROFILING_LIB=${PSYCLONE_PROFILING_DIR}/libroctx_prof.a
#   export PSYCLONE_PROFILING_LIBS="-L${PSYCLONE_PROFILING_DIR} -lroctx_prof"
#   export RUN_CMD="rocprofv3 --runtime-trace --output-format pftrace -- ./traadv.exe"
#   make clean compile run

include ../../common.mk

GENERATED_FILES = traadv_instrumented.F90 \
	traadv_instrumented.o \
	traadv.exe \
	output.dat

RUN_CMD ?= ./traadv.exe

# Profiling wrapper settings
PSYCLONE_PROFILING_DIR ?= $(PSYCLONE_DIR)/lib/profiling/simple_timing
PSYCLONE_PROFILING_INCLUDE ?= ${PSYCLONE_PROFILING_DIR}
PSYCLONE_PROFILING_LIB ?= ${PSYCLONE_PROFILING_DIR}/libsimple_timing.a
PSYCLONE_PROFILING_LIBS ?= -L${PSYCLONE_PROFILING_DIR} -lsimple_timing

transform:
	ENABLE_PROFILING=1 ${PSYCLONE} -s ./omp_gpu_profile_trans.py ../code/tra_adv.F90 -o traadv_instrumented.F90

compile: transform traadv.exe

run: traadv.exe
	IT=10 JPI=64 JPJ=64 JPK=32 ${RUN_CMD}

traadv.exe: traadv_instrumented.o ${PSYCLONE_PROFILING_LIB}
	${F90} ${F90FLAGS} traadv_instrumented.o -o traadv.exe ${LDFLAGS} ${PSYCLONE_PROFILING_LIBS}

traadv_instrumented.o: traadv_instrumented.F90 ${PSYCLONE_PROFILING_LIB}
	${F90} ${F90FLAGS} -I${PSYCLONE_PROFILING_INCLUDE} -c $< -o $@

${PSYCLONE_PROFILING_LIB}:
	${MAKE} -C ${PSYCLONE_PROFILING_DIR} F90=${F90}
