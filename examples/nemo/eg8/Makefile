# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
	${PSYCLONE} -s ./omp_gpu_profile_trans.py --script-kwargs "profiling: True" \
		../code/tra_adv.F90 -o traadv_instrumented.F90

compile: transform traadv.exe

run: transform traadv.exe
	IT=10 JPI=64 JPJ=64 JPK=32 ${RUN_CMD}

traadv.exe: traadv_instrumented.o ${PSYCLONE_PROFILING_LIB}
	${F90} ${F90FLAGS} traadv_instrumented.o -o traadv.exe ${LDFLAGS} ${PSYCLONE_PROFILING_LIBS}

traadv_instrumented.o: traadv_instrumented.F90 ${PSYCLONE_PROFILING_LIB}
	${F90} ${F90FLAGS} -I${PSYCLONE_PROFILING_INCLUDE} -c $< -o $@

${PSYCLONE_PROFILING_LIB}:
	${MAKE} -C ${PSYCLONE_PROFILING_DIR} F90=${F90}
